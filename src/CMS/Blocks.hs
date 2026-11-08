-- | Page builder: blok konten, validasi, dan isi bawaan halaman situs.
module CMS.Blocks
  ( Fields
  , Block (..)
  , Page (..)
  , BlockSpec (..)
  , blockSpecs
  , lookupSpec
  , fieldOf
  , cardVariants
  , cardClasses
  , youtubeId
  , safeUrl
  , decodeBlocks
  , encodeBlocks
  , sanitizeBlocks
  , SitePage (..)
  , sitePages
  , lookupSitePage
  , pagePath
  , postsWanted
  , safeIcon
  , popupFreqs
  , isSystemPage
  , defaultPageIcon
  , PageInput (..)
  , PageTemplate (..)
  , pageTemplates
  , lookupTemplate
  , withPageTitle
  , pageSlugError
  , pageColumns
  ) where

import Control.Monad (forM)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple.FromRow

import CMS.Types (PostSeo (..))
import CMS.Util (readInt)

-- Tipe ----------------------------------------------------------------------------

type Fields = M.Map Text Text

-- | Satu blok: jenisnya, isian teks/atribut, dan daftar item (kartu, cabang, dst.).
data Block = Block
  { bType :: Text
  , bFields :: Fields
  , bItems :: [Fields]
  }

instance FromJSON Block where
  parseJSON = withObject "Block" $ \o ->
    Block <$> o .: "type" <*> o .:? "f" .!= M.empty <*> o .:? "items" .!= []

instance ToJSON Block where
  toJSON b = object ["type" .= bType b, "f" .= bFields b, "items" .= bItems b]

data Page = Page
  { pageSlug :: Text
  , pageTitle :: Text
  , pageBlocks :: [Block]
  , pageUpdated :: Text
  , pagePublished :: Bool -- False = draf, hanya terlihat oleh Admin/Editor
  , pageInMenu :: Bool -- tampil di menu atas, bawah, dan footer
  , pageOrder :: Int -- urutan di menu
  , pageMenuIcon :: Text -- ikon Font Awesome di menu
  , pageSeo :: PostSeo -- judul/deskripsi/gambar SEO & noindex (kata kunci tidak dipakai)
  , pageCreated :: Text
  }

-- | Kolom untuk SELECT, urutannya sama dengan FromRow Page.
pageColumns :: Text
pageColumns =
  "slug, title, blocks, updated_at, status, in_menu, menu_order, icon,\
  \ seo_title, seo_desc, seo_image, seo_noindex, created_at"

instance FromRow Page where
  fromRow =
    Page <$> field <*> field <*> (decodeBlocks <$> field) <*> field
      <*> ((== ("published" :: Text)) <$> field) <*> field <*> field <*> field
      <*> (mkSeo <$> field <*> field <*> field <*> field) <*> field
    where
      mkSeo t d i n = PostSeo t d "" i n

-- | Isian form halaman (buat & pengaturan).
data PageInput = PageInput
  { pgiTitle :: Text
  , pgiSlug :: Text
  , pgiPublished :: Bool
  , pgiInMenu :: Bool
  , pgiIcon :: Text
  , pgiSeo :: PostSeo
  , pgiTemplate :: Text -- hanya saat membuat: kunci templat
  , pgiCopyFrom :: Text -- hanya saat membuat: salin blok dari halaman ini (kosong = pakai templat)
  }

fieldOf :: Text -> Fields -> Text
fieldOf = M.findWithDefault ""

decodeBlocks :: Text -> [Block]
decodeBlocks = fromMaybe [] . decodeStrict . TE.encodeUtf8

encodeBlocks :: [Block] -> Text
encodeBlocks = TE.decodeUtf8 . BL.toStrict . encode

-- Jenis blok ------------------------------------------------------------------------

data BlockSpec = BlockSpec
  { spType :: Text
  , spLabel :: Text
  , spIcon :: Text
  , spFields :: [Text]
  , spItemFields :: [Text]
  , spDefault :: Block -- isi awal saat blok ditambahkan dari panel
  }

blockSpecs :: [BlockSpec]
blockSpecs =
  [ BlockSpec "hero" "Hero" "fa-solid fa-flag" ["eyebrow", "title", "text", "btn", "href", "btn2", "href2", "src", "alt", "layout"] [] $
      blk "hero" [("eyebrow", "Selamat datang"), ("title", "Judul besar untuk halaman Anda"), ("text", "Tulis kalimat pembuka yang singkat dan jelas."), ("btn", "Mulai"), ("href", "/contact")] []
  , BlockSpec "head" "Judul halaman" "fa-solid fa-heading" ["eyebrow", "title", "text"] [] $
      blk "head" [("eyebrow", "Bagian"), ("title", "Judul halaman"), ("text", "Keterangan singkat halaman ini.")] []
  , BlockSpec "text" "Teks" "fa-solid fa-align-left" ["title", "text"] [] $
      blk "text" [("title", "Judul bagian"), ("text", "Tulis paragraf di sini.\n\nTekan Enter dua kali untuk paragraf baru.")] []
  , BlockSpec "features" "Kartu fitur" "fa-solid fa-table-cells-large" ["eyebrow", "title", "text", "style", "cols", "shape", "ishape"] ["icon", "title", "text"] $
      blk "features" [("eyebrow", "Fitur"), ("title", "Kenapa memilih kami"), ("text", "")]
        [ item [("icon", "fa-solid fa-bolt"), ("title", "Cepat"), ("text", "Penjelasan singkat.")]
        , item [("icon", "fa-solid fa-shield-halved"), ("title", "Aman"), ("text", "Penjelasan singkat.")]
        , item [("icon", "fa-solid fa-headset"), ("title", "Dukungan"), ("text", "Penjelasan singkat.")]
        ]
  , BlockSpec "split" "Gambar + teks" "fa-solid fa-image" ["eyebrow", "title", "text", "btn", "href", "src", "alt"] [] $
      blk "split" [("eyebrow", "Tentang"), ("title", "Judul bagian"), ("text", "Ceritakan sesuatu di samping gambar."), ("btn", "Selengkapnya"), ("href", "/about"), ("src", placeholder), ("alt", "")] []
  , BlockSpec "stats" "Angka" "fa-solid fa-chart-simple" [] ["value", "label"] $
      blk "stats" [] [item [("value", "100+"), ("label", "Klien")], item [("value", "10"), ("label", "Tahun")], item [("value", "24/7"), ("label", "Dukungan")]]
  , BlockSpec "cta" "Ajakan (CTA)" "fa-solid fa-bullhorn" ["title", "text", "btn", "href"] [] $
      blk "cta" [("title", "Siap bekerja sama?"), ("text", "Hubungi kami hari ini."), ("btn", "Hubungi kami"), ("href", "/contact")] []
  , BlockSpec "branches" "Daftar cabang" "fa-solid fa-location-dot" ["eyebrow", "title", "text"] ["title", "address", "phone", "hours"] $
      blk "branches" [("eyebrow", "Cabang"), ("title", "Kantor kami"), ("text", "")]
        [item [("title", "Nama cabang"), ("address", "Alamat lengkap"), ("phone", "(021) 000-0000"), ("hours", "Senin–Jumat, 08.00–17.00")]]
  , BlockSpec "posts" "Artikel terbaru" "fa-regular fa-newspaper" ["eyebrow", "title", "count", "btn", "href"] [] $
      blk "posts" [("eyebrow", "Blog"), ("title", "Artikel terbaru"), ("count", "3"), ("btn", "Lihat semua artikel"), ("href", "/blog")] []
  , BlockSpec "contact" "Info kontak" "fa-solid fa-address-card" ["eyebrow", "title", "text", "address", "phone", "email", "hours", "btn"] [] $
      blk "contact" [("eyebrow", "Kontak"), ("title", "Hubungi kami"), ("text", "Kami siap membantu."), ("address", "Alamat kantor"), ("phone", "(021) 000-0000"), ("email", "halo@contoh.id"), ("hours", "Senin–Jumat, 08.00–17.00"), ("btn", "Kirim email")] []
  , BlockSpec "slider" "Slider" "fa-solid fa-images" ["interval"] ["src", "alt", "eyebrow", "title", "text", "btn", "href", "btn2", "href2"] $
      blk "slider" [("interval", "5")]
        [ item [("src", placeholder), ("alt", ""), ("eyebrow", "Slide 1"), ("title", "Judul slide pertama"), ("text", "Tulis keterangan singkat untuk slide ini."), ("btn", "Selengkapnya"), ("href", "/about"), ("btn2", "Hubungi kami"), ("href2", "/contact")]
        , item [("src", placeholder), ("alt", ""), ("eyebrow", "Slide 2"), ("title", "Judul slide kedua"), ("text", "Setiap slide bisa punya gambar, teks, dan tombol sendiri."), ("btn", "Lihat layanan"), ("href", "/services"), ("btn2", ""), ("href2", "")]
        , item [("src", placeholder), ("alt", ""), ("eyebrow", "Slide 3"), ("title", "Judul slide ketiga"), ("text", "Tambah, hapus, atau seret slide untuk mengubah urutannya."), ("btn", "Hubungi kami"), ("href", "/contact"), ("btn2", ""), ("href2", "")]
        ]
  , BlockSpec "image" "Gambar" "fa-regular fa-image" ["src", "alt", "caption"] [] $
      blk "image" [("src", placeholder), ("alt", ""), ("caption", "Keterangan gambar")] []
  , BlockSpec "branchdir" "Direktori cabang" "fa-solid fa-store" ["eyebrow", "title", "text"] [] $
      blk "branchdir" [("eyebrow", "Lokasi kami"), ("title", "Temukan outlet terdekat"), ("text", "Datang langsung ke cabang kami di kota Anda.")] []
  , BlockSpec "testimonials" "Testimoni" "fa-solid fa-quote-left" ["eyebrow", "title", "text"] ["text", "title", "role", "rating", "src", "alt"] $
      blk "testimonials" [("eyebrow", "Testimoni"), ("title", "Kata mereka tentang kami"), ("text", "")]
        [ item [("text", "Pelayanannya cepat dan hasilnya melebihi harapan. Sangat direkomendasikan!"), ("title", "Rina Wijaya"), ("role", "Pemilik, Toko Rina"), ("rating", "5"), ("src", ""), ("alt", "")]
        , item [("text", "Tim yang profesional dan mudah diajak diskusi. Proyek selesai tepat waktu."), ("title", "Budi Santoso"), ("role", "Manajer, PT Maju Jaya"), ("rating", "5"), ("src", ""), ("alt", "")]
        , item [("text", "Dukungan setelah proyek selesai juga sangat membantu kami."), ("title", "Sari Lestari"), ("role", "Direktur, CV Lestari"), ("rating", "4"), ("src", ""), ("alt", "")]
        ]
  , BlockSpec "faq" "Tanya-jawab (FAQ)" "fa-solid fa-circle-question" ["eyebrow", "title", "text"] ["title", "text"] $
      blk "faq" [("eyebrow", "FAQ"), ("title", "Pertanyaan yang sering diajukan"), ("text", "")]
        [ item [("title", "Berapa lama proses pengerjaannya?"), ("text", "Umumnya 2–4 minggu, tergantung kebutuhan. Kami akan memberi jadwal pasti setelah diskusi awal.")]
        , item [("title", "Apakah ada biaya konsultasi?"), ("text", "Konsultasi pertama gratis. Anda baru membayar setelah setuju dengan penawaran kami.")]
        , item [("title", "Bagaimana cara menghubungi tim dukungan?"), ("text", "Hubungi kami lewat WhatsApp, email, atau telepon pada jam kerja.")]
        ]
  , BlockSpec "pricing" "Paket harga" "fa-solid fa-tags" ["eyebrow", "title", "text"] ["title", "price", "period", "text", "features", "btn", "href", "featured"] $
      blk "pricing" [("eyebrow", "Harga"), ("title", "Pilih paket yang sesuai"), ("text", "Harga transparan, tanpa biaya tersembunyi.")]
        [ item [("title", "Dasar"), ("price", "Rp 499rb"), ("period", "/bulan"), ("text", "Untuk usaha yang baru mulai."), ("features", "1 pengguna\nDukungan email\nLaporan bulanan"), ("btn", "Pilih paket"), ("href", "/contact"), ("featured", "")]
        , item [("title", "Bisnis"), ("price", "Rp 999rb"), ("period", "/bulan"), ("text", "Paling banyak dipilih."), ("features", "5 pengguna\nDukungan prioritas\nLaporan mingguan\nKonsultasi bulanan"), ("btn", "Pilih paket"), ("href", "/contact"), ("featured", "1")]
        , item [("title", "Korporat"), ("price", "Hubungi kami"), ("period", ""), ("text", "Solusi khusus untuk perusahaan."), ("features", "Pengguna tak terbatas\nManajer akun khusus\nSLA 24/7"), ("btn", "Konsultasi"), ("href", "/contact"), ("featured", "")]
        ]
  , BlockSpec "team" "Tim" "fa-solid fa-user-group" ["eyebrow", "title", "text"] ["src", "alt", "title", "role", "text"] $
      blk "team" [("eyebrow", "Tim kami"), ("title", "Orang-orang di balik layanan kami"), ("text", "")]
        [ item [("src", placeholder), ("alt", ""), ("title", "Andi Pratama"), ("role", "Direktur"), ("text", "")]
        , item [("src", placeholder), ("alt", ""), ("title", "Dewi Anggraini"), ("role", "Kepala Operasional"), ("text", "")]
        , item [("src", placeholder), ("alt", ""), ("title", "Rizky Hidayat"), ("role", "Pengembang Senior"), ("text", "")]
        , item [("src", placeholder), ("alt", ""), ("title", "Maya Putri"), ("role", "Layanan Pelanggan"), ("text", "")]
        ]
  , BlockSpec "gallery" "Galeri" "fa-regular fa-images" ["eyebrow", "title", "text"] ["src", "alt", "caption"] $
      blk "gallery" [("eyebrow", "Galeri"), ("title", "Dokumentasi kegiatan"), ("text", "")]
        (replicate 6 (item [("src", placeholder), ("alt", ""), ("caption", "")]))
  , BlockSpec "logos" "Logo klien" "fa-regular fa-handshake" ["title"] ["src", "alt"] $
      blk "logos" [("title", "Dipercaya oleh")] (replicate 5 (item [("src", placeholder), ("alt", "Logo klien")]))
  , BlockSpec "video" "Video YouTube" "fa-brands fa-youtube" ["eyebrow", "title", "text", "url"] [] $
      blk "video" [("eyebrow", "Video"), ("title", "Kenali kami dalam 2 menit"), ("text", ""), ("url", "")] []
  , BlockSpec "popup" "Popup gambar" "fa-regular fa-window-restore" ["src", "alt", "btn", "href", "delay", "freq"] [] $
      blk "popup" [("src", placeholder), ("alt", "Brosur promo"), ("btn", "Lihat detail"), ("href", "/contact"), ("delay", "2"), ("freq", "session")] []
  , BlockSpec "code" "Kode custom" "fa-solid fa-code" ["html", "css", "js"] [] $
      blk "code"
        [ ("html", "<div class=\"box\">\n  <h3>Halo!</h3>\n  <p>Ganti dengan HTML Anda.</p>\n  <button id=\"tombol\">Klik saya</button>\n</div>")
        , ("css", ".box {\n  padding: 24px;\n  border-radius: 16px;\n  background: #e2f4f1;\n  text-align: center;\n}")
        , ("js", "document.getElementById(\"tombol\").addEventListener(\"click\", function () {\n  this.textContent = \"Terima kasih!\";\n});")
        ]
        []
  ]

lookupSpec :: Text -> Maybe BlockSpec
lookupSpec t = find ((== t) . spType) blockSpecs

blk :: Text -> [(Text, Text)] -> [Fields] -> Block
blk t fs = Block t (M.fromList fs)

item :: [(Text, Text)] -> Fields
item = M.fromList

placeholder :: Text
placeholder = "/asset/img/placeholder.svg"

-- Validasi ----------------------------------------------------------------------------

-- | Bersihkan data kiriman editor: hanya jenis & isian yang dikenal, URL dan ikon aman.
sanitizeBlocks :: [Block] -> Either Text [Block]
sanitizeBlocks bs
  | length bs > 60 = Left "Terlalu banyak blok (maksimal 60)."
  | otherwise = forM bs $ \b -> case lookupSpec (bType b) of
      Nothing -> Left ("Jenis blok tidak dikenal: " <> T.take 40 (bType b))
      Just sp ->
        Right
          b
            { bFields = clean (spFields sp) (bFields b)
            , bItems = map (clean (spItemFields sp)) (take 40 (bItems b))
            }
  where
    clean allowed m = M.fromList [(k, cleanVal k v) | (k, v) <- M.toList m, k `elem` allowed]
    cleanVal k v
      | k `elem` ["href", "href2", "src", "url"] = safeUrl v
      | k == "rating" = maybe "5" (T.pack . show . max 1 . min 5) (readInt (T.strip v))
      | k == "featured" = if v == "1" then "1" else ""
      | k == "icon" = safeIcon v
      | k == "layout" = if v `elem` ["right", "left", "bg"] then v else "right"
      | Just (def, allowed) <- lookup k cardVariants = if v `elem` allowed then v else def
      | k == "count" = maybe "3" (T.pack . show . max 1 . min 12) (readInt (T.strip v))
      | k == "interval" = maybe "5" (T.pack . show . max 0 . min 30) (readInt (T.strip v))
      | k == "delay" = maybe "2" (T.pack . show . max 0 . min 60) (readInt (T.strip v))
      | k == "freq" = if v `elem` map fst popupFreqs then v else "session"
      | k `elem` ["html", "css", "js"] = T.take 50000 v
      | otherwise = T.take 4000 v

-- | Seberapa sering popup gambar tampil: (nilai, label). Harus sama dengan OPTIONS.freq
-- di static/builder.js dan logika popup di static/app.js.
popupFreqs :: [(Text, Text)]
popupFreqs =
  [ ("session", "Sekali per kunjungan")
  , ("always", "Setiap halaman dibuka")
  , ("day", "Sekali sehari")
  , ("week", "Sekali seminggu")
  ]

-- | Pilihan tampilan kartu fitur: kunci -> (bawaan, nilai yang boleh). Harus sama
-- dengan OPTIONS di static/builder.js dan kelas fs-/fc-/sh-/is- di style.css.
cardVariants :: [(Text, (Text, [Text]))]
cardVariants =
  [ ("style", ("card", ["card", "outline", "lift", "left", "center", "minimal", "accent", "side", "number", "solid", "gradient", "glass", "dark"]))
  , ("cols", ("auto", ["auto", "2", "3", "4"]))
  , ("shape", ("rounded", ["rounded", "sharp", "soft"]))
  , ("ishape", ("rounded", ["rounded", "circle", "plain"]))
  ]

-- | Kelas CSS tampilan kartu untuk isian blok (nilai tidak dikenal = bawaan).
cardClasses :: Fields -> Text
cardClasses fs = T.unwords [pre <> pick k | (k, pre) <- [("style", "fs-"), ("cols", "fc-"), ("shape", "sh-"), ("ishape", "is-")]]
  where
    pick k = case lookup k cardVariants of
      Just (def, allowed) -> let v = fieldOf k fs in if v `elem` allowed then v else def
      Nothing -> ""

-- | Hanya tautan http(s), relatif, anchor, mailto, dan tel. Selain itu dikosongkan.
safeUrl :: Text -> Text
safeUrl raw =
  let u = T.take 1000 (T.strip raw)
      okPrefix = any (`T.isPrefixOf` T.toLower u) ["https://", "http://", "/", "#", "mailto:", "tel:"]
   in if okPrefix && not (T.any (`elem` ['"', '<', '>', ' ']) u) then u else ""

-- | ID video YouTube (11 karakter) dari berbagai bentuk tautan: watch?v=, youtu.be/,
-- /embed/, /shorts/, /live/. Harus sama dengan youtubeId() di static/builder.js.
youtubeId :: Text -> Maybe Text
youtubeId raw
  | not ("youtu" `T.isInfixOf` u) = Nothing
  | otherwise = case [vid | m <- ["v=", "youtu.be/", "/embed/", "/shorts/", "/live/"], let (_, rest) = T.breakOn m u, not (T.null rest), let vid = T.takeWhile ok (T.drop (T.length m) rest), T.length vid == 11] of
      (v : _) -> Just v
      [] -> Nothing
  where
    u = T.strip raw
    ok ch = isAsciiLower ch || isAsciiUpper ch || isDigit ch || ch == '-' || ch == '_'

-- | Kelas ikon Font Awesome saja, mis. "fa-solid fa-truck".
safeIcon :: Text -> Text
safeIcon = T.unwords . take 3 . filter ok . T.words
  where
    ok w = "fa-" `T.isPrefixOf` w && T.all (\ch -> isAsciiLower ch || isDigit ch || ch == '-') w

-- | Jumlah artikel terbanyak yang diminta blok "posts" di halaman.
postsWanted :: [Block] -> Int
postsWanted bs =
  maximum (0 : [maybe 3 (max 1 . min 12) (readInt (fieldOf "count" (bFields b))) | b <- bs, bType b == "posts"])

-- Halaman situs & isi bawaan -------------------------------------------------------------

data SitePage = SitePage
  { spSlug :: Text
  , spTitle :: Text
  , spDefaults :: [Block]
  }

pagePath :: Text -> Text
pagePath "home" = "/"
pagePath s = "/" <> s

lookupSitePage :: Text -> Maybe SitePage
lookupSitePage s = find ((== s) . spSlug) sitePages

sitePages :: [SitePage]
sitePages =
  [ SitePage "home" "Home" $
      [ blk "hero"
          [ ("eyebrow", "Solusi terpercaya untuk bisnis Anda")
          , ("title", "Tumbuh lebih cepat bersama tim yang tepat")
          , ("text", "Kami membantu perusahaan merencanakan, membangun, dan menjalankan layanan yang andal — dari konsultasi hingga dukungan harian.")
          , ("btn", "Lihat layanan"), ("href", "/services")
          , ("btn2", "Hubungi kami"), ("href2", "/contact")
          ] []
      , blk "features" [("eyebrow", "Kenapa kami"), ("title", "Mitra yang fokus pada hasil"), ("text", "Tiga hal yang membuat klien bertahan bersama kami.")]
          [ item [("icon", "fa-solid fa-bolt"), ("title", "Cepat & tanggap"), ("text", "Tim kami merespons kebutuhan Anda dalam hitungan jam, bukan hari.")]
          , item [("icon", "fa-solid fa-shield-halved"), ("title", "Aman & andal"), ("text", "Proses kerja terdokumentasi dengan standar keamanan yang jelas.")]
          , item [("icon", "fa-solid fa-hand-holding-heart"), ("title", "Layanan personal"), ("text", "Satu kontak khusus yang memahami bisnis Anda.")]
          ]
      , blk "stats" [] [item [("value", "250+"), ("label", "Klien aktif")], item [("value", "12"), ("label", "Tahun pengalaman")], item [("value", "8"), ("label", "Kantor cabang")], item [("value", "98%"), ("label", "Klien puas")]]
      , blk "split" [("eyebrow", "Tentang kami"), ("title", "Berawal dari tim kecil, kini melayani ratusan klien"), ("text", "Kami percaya kerja sama yang baik dimulai dari mendengarkan. Itu sebabnya setiap proyek diawali dengan memahami tujuan Anda."), ("btn", "Kenali kami"), ("href", "/about"), ("src", placeholder), ("alt", "Tim kami")] []
      , blk "posts" [("eyebrow", "Blog"), ("title", "Kabar & wawasan terbaru"), ("count", "3"), ("btn", "Lihat semua artikel"), ("href", "/blog")] []
      , blk "cta" [("title", "Siap memulai proyek berikutnya?"), ("text", "Ceritakan kebutuhan Anda, kami bantu cari solusinya."), ("btn", "Hubungi kami"), ("href", "/contact")] []
      ]
  , SitePage "about" "About Us" $
      [ blk "head" [("eyebrow", "About Us"), ("title", "Tentang kami"), ("text", "Kenali cerita, nilai, dan orang-orang di balik layanan kami.")] []
      , blk "split" [("eyebrow", "Cerita kami"), ("title", "Dibangun dari kepercayaan"), ("text", "Didirikan pada 2014, kami memulai dengan satu tujuan sederhana: membantu bisnis lokal bekerja lebih baik.\n\nKini tim kami tersebar di beberapa kota dan melayani klien dari berbagai industri."), ("btn", ""), ("href", ""), ("src", placeholder), ("alt", "Kantor kami")] []
      , blk "features" [("eyebrow", "Nilai kami"), ("title", "Yang kami pegang"), ("text", "")]
          [ item [("icon", "fa-solid fa-handshake"), ("title", "Integritas"), ("text", "Jujur dan transparan dalam setiap langkah.")]
          , item [("icon", "fa-solid fa-lightbulb"), ("title", "Inovasi"), ("text", "Selalu mencari cara yang lebih baik.")]
          , item [("icon", "fa-solid fa-users"), ("title", "Kolaborasi"), ("text", "Hasil terbaik lahir dari kerja sama.")]
          ]
      , blk "stats" [] [item [("value", "2014"), ("label", "Tahun berdiri")], item [("value", "120"), ("label", "Anggota tim")], item [("value", "8"), ("label", "Kota")]]
      , blk "cta" [("title", "Ingin bergabung atau bekerja sama?"), ("text", "Kami senang mendengar dari Anda."), ("btn", "Hubungi kami"), ("href", "/contact")] []
      ]
  , SitePage "services" "Services" $
      [ blk "head" [("eyebrow", "Services"), ("title", "Layanan kami"), ("text", "Solusi lengkap, dari perencanaan sampai dukungan setelah peluncuran.")] []
      , blk "features" [("eyebrow", ""), ("title", "Apa yang bisa kami bantu"), ("text", "")]
          [ item [("icon", "fa-solid fa-compass-drafting"), ("title", "Konsultasi"), ("text", "Analisis kebutuhan dan rencana kerja yang realistis.")]
          , item [("icon", "fa-solid fa-code"), ("title", "Pengembangan"), ("text", "Membangun aplikasi dan situs sesuai kebutuhan bisnis.")]
          , item [("icon", "fa-solid fa-cloud"), ("title", "Infrastruktur"), ("text", "Server dan jaringan yang stabil dan mudah dipantau.")]
          , item [("icon", "fa-solid fa-chart-line"), ("title", "Analitik"), ("text", "Data yang jelas untuk keputusan yang lebih baik.")]
          , item [("icon", "fa-solid fa-graduation-cap"), ("title", "Pelatihan"), ("text", "Membekali tim Anda agar mandiri.")]
          , item [("icon", "fa-solid fa-headset"), ("title", "Dukungan 24/7"), ("text", "Tim siaga kapan pun Anda membutuhkan.")]
          ]
      , blk "split" [("eyebrow", "Cara kerja"), ("title", "Proses yang jelas dari awal"), ("text", "1. Diskusi kebutuhan\n2. Rencana & penawaran\n3. Pengerjaan bertahap\n4. Serah terima & dukungan"), ("btn", "Konsultasi gratis"), ("href", "/contact"), ("src", placeholder), ("alt", "Proses kerja")] []
      , blk "cta" [("title", "Butuh layanan yang tidak ada di daftar?"), ("text", "Ceritakan saja — kemungkinan besar kami bisa membantu."), ("btn", "Diskusikan"), ("href", "/contact")] []
      ]
  , SitePage "branch" "Branch" $
      [ blk "head" [("eyebrow", "Branch"), ("title", "Kantor cabang"), ("text", "Temukan kantor terdekat dari lokasi Anda.")] []
      , blk "branchdir" [("eyebrow", ""), ("title", "Lokasi kami"), ("text", "")] []
      , blk "cta" [("title", "Tidak ada cabang di kota Anda?"), ("text", "Tim kami juga melayani secara online."), ("btn", "Hubungi kami"), ("href", "/contact")] []
      ]
  , SitePage "blog" "Blog" $
      [ blk "head" [("eyebrow", "Blog"), ("title", "Kabar & wawasan"), ("text", "Tulisan terbaru dari tim kami.")] []
      ]
  , SitePage "contact" "Contact" $
      [ blk "head" [("eyebrow", "Contact"), ("title", "Hubungi kami"), ("text", "Punya pertanyaan? Kami siap membantu.")] []
      , blk "contact" [("eyebrow", ""), ("title", "Mari bicara"), ("text", "Kirim email atau telepon kami pada jam kerja. Kami biasanya membalas dalam satu hari kerja."), ("address", "Jl. Sudirman No. 10, Jakarta Pusat"), ("phone", "(021) 555-0101"), ("email", "halo@contoh.id"), ("hours", "Senin–Jumat, 08.00–17.00"), ("btn", "Kirim email")] []
      ]
  ]

-- Halaman buatan pengguna ------------------------------------------------------------------

-- | Halaman bawaan (Home, About Us, …): tidak bisa dihapus dan alamatnya tetap.
isSystemPage :: Text -> Bool
isSystemPage s = any ((== s) . spSlug) sitePages

-- | Ikon menu bawaan untuk halaman bawaan; halaman baru memakai ikon berkas.
defaultPageIcon :: Text -> Text
defaultPageIcon = \case
  "home" -> "fa-solid fa-house"
  "about" -> "fa-solid fa-users"
  "services" -> "fa-solid fa-briefcase"
  "branch" -> "fa-solid fa-location-dot"
  "blog" -> "fa-regular fa-newspaper"
  "contact" -> "fa-solid fa-envelope"
  _ -> "fa-regular fa-file-lines"

-- | Alamat yang dipakai rute lain atau berkas sistem, tidak boleh jadi slug halaman.
reservedSlugs :: [Text]
reservedSlugs =
  [ "home", "blog", "admin", "api", "static", "asset", "post", "category", "login", "register", "logout"
  , "sitemap.xml", "robots.txt", "favicon.ico", "new", "search", "feed", "rss", "uploads"
  ]

-- | Pesan kesalahan untuk slug halaman baru, atau Nothing bila sah.
pageSlugError :: Text -> Maybe Text
pageSlugError s
  | T.null s = Just "Alamat halaman wajib diisi."
  | T.length s > 60 = Just "Alamat halaman maksimal 60 karakter."
  | not (T.all ok s) || "-" `T.isPrefixOf` s || "-" `T.isSuffixOf` s =
      Just "Alamat hanya boleh huruf kecil, angka, dan tanda hubung (mis. promo-akhir-tahun)."
  | s `elem` reservedSlugs = Just ("Alamat “/" <> s <> "” dipakai sistem. Pilih alamat lain.")
  | otherwise = Nothing
  where
    ok ch = isAsciiLower ch || isDigit ch || ch == '-'

-- | Templat awal halaman baru.
data PageTemplate = PageTemplate
  { ptKey :: Text
  , ptLabel :: Text
  , ptDesc :: Text
  , ptIcon :: Text
  , ptBlocks :: [Block]
  }

lookupTemplate :: Text -> Maybe PageTemplate
lookupTemplate k = find ((== k) . ptKey) pageTemplates

pageTemplates :: [PageTemplate]
pageTemplates =
  [ PageTemplate "blank" "Kosong" "Hanya judul halaman. Susun sendiri dari nol." "fa-regular fa-file" [headBlk]
  , PageTemplate "basic" "Halaman standar" "Judul, teks, dan ajakan di bawah. Cocok untuk info atau kebijakan." "fa-solid fa-align-left" $
      [headBlk] ++ spec "text" ++ spec "cta"
  , PageTemplate "landing" "Landing page" "Hero, fitur, angka, testimoni, dan ajakan. Untuk promosi atau produk." "fa-solid fa-rocket" $
      concatMap spec ["hero", "features", "stats", "testimonials", "cta"]
  , PageTemplate "about" "Tentang" "Cerita, nilai, angka, dan tim." "fa-solid fa-users" $
      systemDefaults "about" ++ spec "team"
  , PageTemplate "services" "Layanan" "Daftar layanan, cara kerja, dan ajakan." "fa-solid fa-briefcase" (systemDefaults "services")
  , PageTemplate "pricing" "Harga" "Paket harga, FAQ, dan ajakan." "fa-solid fa-tags" $
      [headBlk] ++ concatMap spec ["pricing", "faq", "cta"]
  , PageTemplate "faq" "FAQ" "Tanya-jawab lengkap dengan data terstruktur Google." "fa-solid fa-circle-question" $
      [headBlk] ++ concatMap spec ["faq", "cta"]
  , PageTemplate "gallery" "Galeri & video" "Foto kegiatan, logo klien, dan video." "fa-regular fa-images" $
      [headBlk] ++ concatMap spec ["gallery", "video", "logos"]
  , PageTemplate "contact" "Kontak" "Info kontak dan daftar cabang." "fa-solid fa-envelope" $
      systemDefaults "contact" ++ spec "branches"
  ]
  where
    headBlk = blk "head" [("eyebrow", ""), ("title", "Judul halaman"), ("text", "Keterangan singkat halaman ini.")] []
    spec t = maybe [] (pure . spDefault) (lookupSpec t)
    systemDefaults s = maybe [headBlk] spDefaults (lookupSitePage s)

-- | Pakai judul halaman di blok judul/hero pertama templat, supaya halaman baru
-- langsung berjudul benar.
withPageTitle :: Text -> [Block] -> [Block]
withPageTitle title = go
  where
    go [] = []
    go (b : bs)
      | bType b `elem` ["head", "hero"] = b {bFields = M.insert "title" title (bFields b)} : bs
      | otherwise = b : go bs
