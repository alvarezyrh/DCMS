-- | Tipe domain, konfigurasi, dan konteks request.
module CMS.Types where

import Control.Concurrent.MVar (MVar)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import CMS.RateLimit (Limiter)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField
import Network.HTTP.Types (Method)
import Data.Time.Clock (UTCTime)
import Network.Wai (Request)
import Network.Wai.Parse (FileInfo)
import qualified Data.ByteString.Lazy as BL

-- Peran -----------------------------------------------------------------

data Role = RAdmin | REditor | RUser
  deriving (Eq, Ord, Show, Enum, Bounded)

roleText :: Role -> Text
roleText = \case
  RAdmin -> "admin"
  REditor -> "editor"
  RUser -> "user"

roleLabel :: Role -> Text
roleLabel = \case
  RAdmin -> "Administrator"
  REditor -> "Editor"
  RUser -> "User"

roleFromText :: Text -> Maybe Role
roleFromText t = lookup t [(roleText r, r) | r <- [minBound .. maxBound]]

instance FromField Role where
  fromField f =
    fromField f >>= \t ->
      maybe (returnError ConversionFailed f ("role: " <> T.unpack t)) Ok (roleFromText t)

instance ToField Role where
  toField = toField . roleText

-- PostStatus artikel ----------------------------------------------------------

data PostStatus = Draft | Pending | Published
  deriving (Eq, Ord, Show, Enum, Bounded)

statusText :: PostStatus -> Text
statusText = \case
  Draft -> "draft"
  Pending -> "pending"
  Published -> "published"

statusLabel :: PostStatus -> Text
statusLabel = \case
  Draft -> "Draf"
  Pending -> "Menunggu review"
  Published -> "Terbit"

statusFromText :: Text -> Maybe PostStatus
statusFromText t = lookup t [(statusText s, s) | s <- [minBound .. maxBound]]

instance FromField PostStatus where
  fromField f =
    fromField f >>= \t ->
      maybe (returnError ConversionFailed f ("status: " <> T.unpack t)) Ok (statusFromText t)

instance ToField PostStatus where
  toField = toField . statusText

-- Entitas -----------------------------------------------------------------

data User = User
  { userId :: Int
  , userName :: Text
  , userEmail :: Text
  , userHash :: Text
  , userRole :: Role
  , userDisplay :: Text
  , userBio :: Text
  , userActive :: Bool
  , userCreated :: Text
  , userAvatar :: Text -- URL foto profil, kosong bila belum ada
  , userDashHidden :: Text -- widget dashboard yang disembunyikan, dipisah koma
  }

instance FromRow User where
  fromRow =
    User <$> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field <*> field

-- | Nama yang ditampilkan: display name, atau username bila kosong.
userShownName :: User -> Text
userShownName u = if T.null (userDisplay u) then userName u else userDisplay u

data Category = Category
  { catId :: Int
  , catName :: Text
  , catSlug :: Text
  , catDesc :: Text
  , catCount :: Int -- jumlah artikel terbit
  }

instance FromRow Category where
  fromRow = Category <$> field <*> field <*> field <*> field <*> field

data Post = Post
  { postId :: Int
  , postTitle :: Text
  , postSlug :: Text
  , postExcerpt :: Text
  , postBody :: Text
  , postStatus :: PostStatus
  , postAuthorId :: Int
  , postAuthorName :: Text
  , postCategoryId :: Maybe Int
  , postCategoryName :: Maybe Text
  , postCategorySlug :: Maybe Text
  , postCreated :: Text
  , postUpdated :: Text
  , postPublished :: Maybe Text
  , postSeo :: PostSeo
  }

-- | Isian SEO per artikel. Kosong = otomatis (judul, ringkasan, gambar pertama).
data PostSeo = PostSeo
  { seoTitle :: Text
  , seoDesc :: Text
  , seoKeyword :: Text -- kata kunci utama
  , seoImage :: Text -- gambar pratinjau saat dibagikan
  , seoNoindex :: Bool -- jangan tampilkan di mesin pencari
  }

emptySeo :: PostSeo
emptySeo = PostSeo "" "" "" "" False

instance FromRow Post where
  fromRow =
    Post <$> field <*> field <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field <*> field <*> field
      <*> (PostSeo <$> field <*> field <*> field <*> field <*> field)

-- | Cabang/outlet (menu Cabang di dashboard, tampil di halaman /branch).
data Branch = Branch
  { brId :: Int
  , brName :: Text -- nama outlet, mis. "Pondok Indah"
  , brCity :: Text -- kota/wilayah, mis. "Jakarta Selatan"
  , brProvince :: Text
  , brAddress :: Text
  , brPhone :: Text
  , brPhone2 :: Text
  , brWhatsapp :: Text -- nomor WA format internasional tanpa "+"; kosong = dari telepon
  , brHours :: Text -- mis. "10.00–21.00 WIB"
  , brNote :: Text -- mis. "Buka setiap hari"
  , brMapsUrl :: Text -- tautan "Petunjuk arah" (Google Maps)
  , brMapsQuery :: Text -- kata kunci peta untuk sematan, mis. "Raja Emas Pondok Indah"
  , brSort :: Int
  , brActive :: Bool
  , brUpdated :: Text
  , brLat :: Maybe Double -- koordinat untuk urutan "terdekat"
  , brLng :: Maybe Double
  }

instance FromRow Branch where
  fromRow =
    Branch <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- | Isian form cabang (buat & ubah) dan data awal.
data BranchInput = BranchInput
  { biName, biCity, biProvince, biAddress, biPhone, biPhone2, biWhatsapp, biHours, biNote, biMapsUrl, biMapsQuery :: Text
  , biSort :: Int
  , biActive :: Bool
  , biLat, biLng :: Maybe Double
  }

emptyBranchInput :: BranchInput
emptyBranchInput = BranchInput "" "" "" "" "" "" "" "" "Buka setiap hari" "" "" 0 True Nothing Nothing

branchToInput :: Branch -> BranchInput
branchToInput Branch {..} =
  BranchInput brName brCity brProvince brAddress brPhone brPhone2 brWhatsapp brHours brNote brMapsUrl brMapsQuery brSort brActive brLat brLng

-- | API key untuk API Gateway. Key asli hanya ditampilkan sekali saat dibuat; yang
-- disimpan hanya hash SHA-256 dan awalannya (untuk dikenali di daftar).
data ApiKey = ApiKey
  { akId :: Int
  , akName :: Text
  , akPrefix :: Text -- mis. "dcms_3f9c1a"
  , akHash :: Text
  , akScopes :: Text -- "*" = semua API, atau daftar dipisah koma: "branches,posts"
  , akRateLimit :: Int -- permintaan per menit
  , akActive :: Bool
  , akCreated :: Text
  , akLastUsed :: Maybe Text
  }

instance FromRow ApiKey where
  fromRow = ApiKey <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- | Daftar provinsi Indonesia (pilihan di form cabang).
provinces :: [Text]
provinces =
  [ "Aceh", "Sumatera Utara", "Sumatera Barat", "Riau", "Kepulauan Riau", "Jambi", "Bengkulu", "Sumatera Selatan"
  , "Kepulauan Bangka Belitung", "Lampung", "Banten", "DKI Jakarta", "Jawa Barat", "Jawa Tengah", "DI Yogyakarta"
  , "Jawa Timur", "Bali", "Nusa Tenggara Barat", "Nusa Tenggara Timur", "Kalimantan Barat", "Kalimantan Tengah"
  , "Kalimantan Selatan", "Kalimantan Timur", "Kalimantan Utara", "Sulawesi Utara", "Gorontalo", "Sulawesi Tengah"
  , "Sulawesi Barat", "Sulawesi Selatan", "Sulawesi Tenggara", "Maluku", "Maluku Utara", "Papua", "Papua Barat"
  ]

-- | Data form artikel (untuk buat & ubah).
data PostInput = PostInput
  { piTitle :: Text
  , piSlug :: Text
  , piExcerpt :: Text
  , piBody :: Text
  , piStatus :: PostStatus
  , piCategory :: Maybe Int
  , piSeo :: PostSeo
  }

emptyPostInput :: PostInput
emptyPostInput = PostInput "" "" "" "" Draft Nothing emptySeo

postToInput :: Post -> PostInput
postToInput p =
  PostInput (postTitle p) (postSlug p) (postExcerpt p) (postBody p) (postStatus p) (postCategoryId p) (postSeo p)

data Stats = Stats
  { stTotal :: Int
  , stPublished :: Int
  , stPending :: Int
  , stDraft :: Int
  , stUsers :: Int
  , stCategories :: Int
  }

data Settings = Settings
  { setSiteName :: Text
  , setTagline :: Text
  , setAllowRegister :: Bool
  , setPerPage :: Int
  , setNav :: NavStyle -- warna & jarak navigasi, diatur dari page builder
  , setDescription :: Text -- deskripsi situs untuk mesin pencari & pratinjau tautan
  , setWhatsapp :: Text -- nomor WhatsApp format internasional tanpa "+", mis. 628123456789
  , setWaMessage :: Text -- pesan awal saat tombol WhatsApp diklik
  , setSocial :: [(Text, Text)] -- (jenis, URL) media sosial, lihat socialKinds
  , setSeo :: SiteSeo
  , setLogo :: Text -- URL logo situs (/asset/img/logo-…), kosong = tanpa logo
  }

-- | Integrasi SEO situs (menu SEO, khusus Administrator).
data SiteSeo = SiteSeo
  { ssGoogleVerify :: Text -- kode verifikasi Google Search Console
  , ssBingVerify :: Text -- kode verifikasi Bing Webmaster Tools
  , ssGaId :: Text -- ID pengukuran Google Analytics 4, mis. G-ABC123
  , ssDefaultImage :: Text -- gambar pratinjau bawaan saat tautan dibagikan
  , ssIndexable :: Bool -- izinkan mesin pencari mengindeks situs
  }

emptySiteSeo :: SiteSeo
emptySiteSeo = SiteSeo "" "" "" "" True

-- | Media sosial yang didukung: (kunci, label, ikon Font Awesome).
socialKinds :: [(Text, Text, Text)]
socialKinds =
  [ ("instagram", "Instagram", "fa-brands fa-instagram")
  , ("facebook", "Facebook", "fa-brands fa-facebook-f")
  , ("x", "X (Twitter)", "fa-brands fa-x-twitter")
  , ("youtube", "YouTube", "fa-brands fa-youtube")
  , ("tiktok", "TikTok", "fa-brands fa-tiktok")
  , ("linkedin", "LinkedIn", "fa-brands fa-linkedin-in")
  ]

-- | Kunci → nilai (warna "#rrggbb" atau angka); lihat CMS.NavStyle.
type NavStyle = M.Map Text Text

defaultSettings :: Settings
defaultSettings = Settings "DCMS" "CMS sederhana yang ditulis dengan Haskell" True 10 M.empty "" "" "" [] emptySiteSeo ""

-- Lingkungan & konteks ------------------------------------------------------

data Env = Env
  { envDb :: MVar Connection
  , envStaticDir :: FilePath
  , envAssetDir :: FilePath -- folder gambar (src/asset/img), disajikan di /asset/img/
  , envDbPath :: FilePath
  , envStarted :: UTCTime -- waktu server mulai (untuk uptime)
  , envLimiter :: Limiter -- pembatas percobaan login
  , envSiteUrl :: Maybe Text -- CMS_SITE_URL, mis. "https://contoh.id" (alamat resmi situs)
  , envSecure :: Bool -- situs diakses lewat HTTPS: cookie Secure + HSTS
  , envTrustProxy :: Bool -- CMS_TRUST_PROXY=1: IP pengunjung dari X-Forwarded-For
  , envApiCounter :: MVar (M.Map Text (Integer, Int)) -- batas permintaan API per menit
  }

data Flash = Flash
  { flashKind :: Text -- "success" | "error" | "info"
  , flashMsg :: Text
  }

-- | Semua yang diperlukan handler untuk satu request.
data Ctx = Ctx
  { cEnv :: Env
  , cReq :: Request
  , cMethod :: Method
  , cPath :: [Text]
  , cQuery :: [(Text, Text)]
  , cForm :: [(Text, Text)]
  , cFiles :: [(Text, FileInfo BL.ByteString)] -- berkas dari form multipart
  , cUser :: Maybe User
  , cSession :: Maybe Text -- token sesi mentah dari cookie
  , cCsrf :: Text
  , cFlash :: Maybe Flash
  , cSettings :: Settings
  , cMenu :: [(Text, Text, Text)] -- menu situs: (slug, judul, ikon) halaman terbit
  , cYear :: Text -- tahun berjalan (waktu lokal server), untuk {tahun} di footer
  }

-- Izin ----------------------------------------------------------------------

-- | Admin dan editor boleh mengelola semua konten.
isStaff :: User -> Bool
isStaff u = userRole u `elem` [RAdmin, REditor]

isAdmin :: User -> Bool
isAdmin u = userRole u == RAdmin

canEditPost :: User -> Post -> Bool
canEditPost u p = isStaff u || postAuthorId p == userId u

-- | PostStatus yang boleh dipilih oleh peran ini.
allowedStatuses :: User -> [PostStatus]
allowedStatuses u
  | isStaff u = [Draft, Pending, Published]
  | otherwise = [Draft, Pending]
