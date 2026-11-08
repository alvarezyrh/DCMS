-- | Katalog API & konfigurasi API Gateway (dipakai handler API, dashboard, dokumentasi,
-- dan spesifikasi OpenAPI agar semuanya selalu sama).
module CMS.Api
  ( ApiDef (..)
  , Endpoint (..)
  , ApiConf (..)
  , apiCatalog
  , lookupApi
  , apiEnabled
  , apiPublic
  , decodeApiConf
  , encodeApiConf
  , corsOrigins
  , defaultRateLimit
  , publicRateLimit
  ) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data Endpoint = Endpoint
  { epPath :: Text -- mis. "/api/v1/branches/{id}"
  , epSummary :: Text
  , epParams :: [(Text, Text, Text)] -- (nama, tipe, keterangan); parameter path ditulis {nama}
  }

data ApiDef = ApiDef
  { adKey :: Text -- dipakai di URL & cakupan key
  , adName :: Text
  , adIcon :: Text
  , adDesc :: Text
  , adEndpoints :: [Endpoint]
  }

apiCatalog :: [ApiDef]
apiCatalog =
  [ ApiDef "branches" "Cabang" "fa-solid fa-store" "Daftar cabang/outlet: alamat, kontak, jam buka, peta, dan urutan terdekat." $
      [ Endpoint "/api/v1/branches" "Daftar cabang aktif" $
          [ ("q", "string", "Cari di nama, kota, provinsi, alamat, telepon")
          , ("province", "string", "Filter provinsi, mis. DKI Jakarta")
          , ("city", "string", "Filter kota, mis. Surabaya")
          , ("near", "string", "Urutkan dari terdekat ke titik \"lat,lng\", mis. -6.2088,106.8456 (menambah distance_km)")
          , ("radius_km", "number", "Bersama near: hanya cabang dalam radius ini")
          , ("page", "integer", "Halaman (mulai 1)")
          , ("per_page", "integer", "Jumlah per halaman (1–200, bawaan 50)")
          ]
      , Endpoint "/api/v1/branches/{id}" "Detail satu cabang" [("id", "integer", "ID cabang")]
      , Endpoint "/api/v1/branches/provinces" "Jumlah cabang per provinsi" []
      ]
  , ApiDef "posts" "Artikel" "fa-regular fa-newspaper" "Artikel yang sudah terbit, lengkap dengan isi (Markdown & HTML) dan data SEO." $
      [ Endpoint "/api/v1/posts" "Daftar artikel terbit (terbaru dulu)" $
          [ ("q", "string", "Cari di judul & isi")
          , ("category", "string", "Slug kategori, mis. tutorial")
          , ("page", "integer", "Halaman (mulai 1)")
          , ("per_page", "integer", "Jumlah per halaman (1–50, bawaan 10)")
          ]
      , Endpoint "/api/v1/posts/{slug}" "Detail artikel (dengan isi lengkap)" [("slug", "string", "Slug artikel")]
      ]
  , ApiDef "categories" "Kategori" "fa-solid fa-tags" "Kategori artikel beserta jumlah artikel terbitnya." $
      [Endpoint "/api/v1/categories" "Daftar kategori" []]
  , ApiDef "site" "Info situs" "fa-solid fa-circle-info" "Nama, tagline, deskripsi, logo, WhatsApp, dan media sosial situs." $
      [Endpoint "/api/v1/site" "Info situs" []]
  ]

lookupApi :: Text -> Maybe ApiDef
lookupApi k = case filter ((== k) . adKey) apiCatalog of
  (a : _) -> Just a
  [] -> Nothing

-- | Batas bawaan permintaan per menit untuk API key baru, dan untuk akses publik per IP.
defaultRateLimit, publicRateLimit :: Int
defaultRateLimit = 60
publicRateLimit = 60

-- Konfigurasi ---------------------------------------------------------------------------

data ApiConf = ApiConf
  { acApis :: M.Map Text (Bool, Bool) -- api -> (aktif, publik tanpa key)
  , acCors :: Text -- "*" atau daftar origin, satu per baris
  }

-- | Bawaan: semua API aktif dan wajib API key.
apiEnabled, apiPublic :: ApiConf -> Text -> Bool
apiEnabled conf k = maybe True fst (M.lookup k (acApis conf))
apiPublic conf k = maybe False snd (M.lookup k (acApis conf))

decodeApiConf :: Text -> ApiConf
decodeApiConf t = fromMaybe (ApiConf M.empty "*") $ do
  v <- decodeStrict (TE.encodeUtf8 t) :: Maybe Value
  flip parseMaybe v $ withObject "conf" $ \o -> do
    apis <- o .:? "apis" .!= M.empty
    cors <- o .:? "cors" .!= "*"
    pairs <- traverse (withObject "api" (\a -> (,) <$> a .:? "enabled" .!= True <*> a .:? "public" .!= False)) (apis :: M.Map Text Value)
    pure (ApiConf (M.filterWithKey (\k _ -> k `elem` map adKey apiCatalog) pairs) (if T.null (T.strip cors) then "*" else cors))

encodeApiConf :: ApiConf -> Text
encodeApiConf (ApiConf apis cors) =
  TE.decodeUtf8 . BL.toStrict . encode $
    object
      [ "apis" .= M.map (\(e, p) -> object ["enabled" .= e, "public" .= p]) apis
      , "cors" .= cors
      ]

-- | Nothing = semua origin boleh ("*"); Just xs = hanya origin ini.
corsOrigins :: ApiConf -> Maybe [Text]
corsOrigins conf =
  let xs = filter (not . T.null) (map (T.dropWhileEnd (== '/') . T.strip) (T.split (`elem` [',', '\n', '\r', ' ']) (acCors conf)))
   in if null xs || "*" `elem` xs then Nothing else Just xs
