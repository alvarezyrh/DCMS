-- | Pengaturan tampilan situs dari page builder: gaya, ukuran, logo website,
-- navigasi (atas & bawah dalam satu pengaturan), dan footer.
--
-- Disimpan sebagai satu JSON di tabel settings (kunci "nav_style") dan dipakai di
-- seluruh situs. Nilainya dipasang sebagai variabel CSS di :root; static/style.css
-- memakai variabel itu dengan nilai bawaan sebagai cadangan, jadi isian yang kosong
-- berarti tampilan bawaan.
module CMS.NavStyle
  ( NavStyle
  , NavKind (..)
  , NavField (..)
  , NavGroup (..)
  , navGroups
  , sanitizeNav
  , decodeNav
  , encodeNav
  , navCss
  , navBottomOn
  , lightboxOn
  , revealOn
  , toTopOn
  , fontLink
  , fontFamilies
  , navText
  , navFlag
  , siteLogo
  , pageBoxOn
  ) where

import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString.Lazy as BL
import Control.Applicative ((<|>))
import Data.Char (isControl, isHexDigit)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (readHex)

import CMS.Types (NavStyle)
import CMS.Util (readInt)

data NavKind
  = NavColor
  | NavRange Int Int Int Text -- min, max, bawaan, satuan
  | NavToggle Bool -- bawaan; disimpan "1" / "0"
  | NavSelect Text [(Text, Text)] -- bawaan, pilihan (nilai, label)
  | NavText Int Bool Text -- maks. karakter, banyak baris?, contoh isian
  | NavImage -- URL gambar (unggahan /asset/img/… atau https://)
  | NavHeading -- sub-judul di panel, tanpa nilai

data NavField = NavField
  { nfKey :: Text
  , nfLabel :: Text
  , nfKind :: NavKind
  }

data NavGroup = NavGroup
  { ngId :: Text
  , ngTitle :: Text
  , ngIcon :: Text
  , ngFields :: [NavField]
  }

navGroups :: [NavGroup]
navGroups =
  [ NavGroup "theme" "Tema" "fa-solid fa-palette"
      [ NavField "h_style" "Warna & huruf" NavHeading
      , NavField "theme_primary" "Warna utama (tombol, tautan, aksen)" NavColor
      , NavField "theme_bg" "Warna latar website (body)" NavColor
      , NavField "theme_page" "Warna halaman (kotak isi selebar website)" NavColor
      , NavField "page_radius" "Sudut halaman" (NavRange 0 40 0 "px")
      , NavField "theme_font" "Jenis huruf" (NavSelect "nunito" [(k, l) | (k, l, _, _) <- fontFamilies])
      , NavField "theme_reveal" "Animasi muncul saat digulir" (NavToggle True)
      , NavField "theme_totop" "Tombol kembali ke atas" (NavToggle True)
      , NavField "h_size" "Ukuran website" NavHeading
      , NavField "site_width" "Lebar maksimal konten" (NavRange 720 1920 1140 "px")
      , NavField "site_pad" "Jarak tepi kiri-kanan" (NavRange 8 80 20 "px")
      , NavField "site_lightbox" "Popup gambar saat diklik" (NavToggle True)
      , NavField "h_logo" "Logo website" NavHeading
      , NavField "site_logo" "Logo di menu atas & footer situs" NavImage
      , NavField "logo_h" "Tinggi logo" (NavRange 16 96 30 "px")
      , NavField "brand_name" "Tampilkan nama situs di samping logo" (NavToggle True)
      ]
  , NavGroup "nav" "Navigasi" "fa-solid fa-bars" $
      [ NavField "h_nav" "Warna menu atas & bawah" NavHeading
      , NavField "nav_bg" "Warna latar" NavColor
      , NavField "nav_opacity" "Kepekatan latar" (NavRange 0 100 90 "%")
      , NavField "nav_text" "Warna teks menu" NavColor
      , NavField "nav_active" "Warna menu aktif" NavColor
      , NavField "h_top" "Menu atas" NavHeading
      , NavField "top_full" "Lebar penuh (dari tepi ke tepi)" (NavToggle False)
      , NavField "top_width" "Panjang bar (bawaan = lebar website)" (NavRange 480 1920 1140 "px")
      ]
        ++ sizes "top" "Jarak dari atas" 48 12 32 9 32 2
        ++ [ NavField "top_radius" "Kelengkungan sudut" (NavRange 0 40 22 "px")
           , NavField "h_bot" "Menu bawah (HP & saat digulir)" NavHeading
           , NavField "bot_enabled" "Tampilkan menu bawah" (NavToggle True)
           ]
        ++ sizes "bot" "Jarak dari bawah" 60 14 24 5 24 2
        ++ [NavField "bot_icon" "Ukuran ikon" (NavRange 10 28 18 "px")]
  , NavGroup "footer" "Footer" "fa-solid fa-window-minimize"
      [ NavField "h_ft_content" "Isi" NavHeading
      , NavField "footer_text" "Teks footer" (NavText 300 True "Kosong = slogan situs")
      , NavField "footer_copy" "Teks hak cipta" (NavText 120 False "Kosong = © Nama situs. Tulis {tahun} untuk tahun berjalan")
      , NavField "footer_layout" "Tata letak" (NavSelect "split" [("split", "Kiri-kanan"), ("center", "Rata tengah")])
      , NavField "footer_brand" "Tampilkan logo & nama" (NavToggle True)
      , NavField "footer_menu" "Tampilkan menu halaman" (NavToggle True)
      , NavField "footer_social" "Tampilkan ikon media sosial" (NavToggle True)
      , NavField "h_ft_color" "Warna" NavHeading
      , NavField "footer_bg" "Warna latar" NavColor
      , NavField "footer_fg" "Warna teks" NavColor
      , NavField "footer_link" "Warna tautan & judul" NavColor
      , NavField "h_ft_size" "Ukuran & bentuk" NavHeading
      , NavField "footer_float" "Footer melayang (floating)" (NavToggle False)
      , NavField "footer_width" "Lebar footer melayang" (NavRange 480 1920 1180 "px")
      , NavField "footer_offset" "Jarak melayang dari tepi" (NavRange 0 60 16 "px")
      , NavField "footer_pad" "Jarak dalam atas-bawah" (NavRange 8 120 40 "px")
      , NavField "footer_radius" "Kelengkungan sudut" (NavRange 0 48 24 "px")
      ]
  ]
  where
    sizes p offLabel offMax offDef padMax padDef gapMax gapDef =
      [ NavField (p <> "_pad") "Jarak dalam (tinggi bar)" (NavRange 0 padMax padDef "px")
      , NavField (p <> "_gap") "Jarak antar menu" (NavRange 0 gapMax gapDef "px")
      , NavField (p <> "_offset") offLabel (NavRange 0 offMax offDef "px")
      ]

allFields :: [NavField]
allFields = concatMap ngFields navGroups

-- | Buang kunci yang tidak dikenal dan nilai yang tidak valid; angka dibatasi ke rentangnya.
sanitizeNav :: M.Map Text Text -> NavStyle
sanitizeNav m = M.fromList (mapMaybe one allFields)
  where
    one f = do
      v <- T.strip <$> M.lookup (nfKey f) m
      (,) (nfKey f) <$> case nfKind f of
        NavColor -> cleanColor v
        NavRange lo hi _ _ -> T.pack . show . max lo . min hi <$> readInt v
        NavToggle _ -> if v `elem` ["0", "1"] then Just v else Nothing
        NavSelect _ opts -> if v `elem` map fst opts then Just v else Nothing
        NavText mx multi _ ->
          let keep ch = (multi && ch == '\n') || not (isControl ch)
              t = T.strip (T.take mx (T.filter keep (T.replace "\r\n" "\n" v)))
           in if T.null t then Nothing else Just t
        NavImage -> cleanImage v
        NavHeading -> Nothing

-- | Hanya gambar unggahan situs (/asset/img/…) atau https://, tanpa karakter berbahaya.
cleanImage :: Text -> Maybe Text
cleanImage v
  | T.length v > 500 || T.any (`elem` ['"', '\'', '<', '>', ' ', '(', ')', '\\']) v = Nothing
  | "/asset/img/" `T.isPrefixOf` v || "https://" `T.isPrefixOf` T.toLower v = Just v
  | otherwise = Nothing

cleanColor :: Text -> Maybe Text
cleanColor v = case T.uncons (T.toLower v) of
  Just ('#', h) | T.length h == 6 && T.all isHexDigit h -> Just ("#" <> T.toLower h)
  _ -> Nothing

decodeNav :: Text -> NavStyle
decodeNav = sanitizeNav . legacyColors . fromMaybe M.empty . decodeStrict . TE.encodeUtf8

-- | Versi lama menyimpan warna navigasi atas & bawah terpisah (top_bg, bot_bg, …).
-- Sekarang satu warna untuk keduanya: pakai warna menu atas (atau bawah) yang lama.
legacyColors :: M.Map Text Text -> M.Map Text Text
legacyColors m =
  M.union m $
    M.fromList
      [ ("nav_" <> k, v)
      | k <- ["bg", "opacity", "text", "active"]
      , not (M.member ("nav_" <> k) m)
      , Just v <- [M.lookup ("top_" <> k) m <|> M.lookup ("bot_" <> k) m]
      ]

encodeNav :: NavStyle -> Text
encodeNav = TE.decodeUtf8 . BL.toStrict . encode

-- | Isi elemen <style>: variabel CSS untuk nilai yang diisi saja. Logika yang sama
-- dipakai static/builder.js untuk pratinjau langsung.
navCss :: NavStyle -> Text
navCss s
  | null vars = ""
  | otherwise = ":root{" <> T.concat [k <> ":" <> v <> ";" | (k, v) <- vars] <> "}"
  where
    vars = themeVars ++ siteVars ++ concatMap groupVars [("top", "--nt"), ("bot", "--nb")] ++ footerVars
    themeVars =
      concat
        [ case get "theme_primary" of
            Just c ->
              [ ("--primary", c)
              , ("--primary-soft", "color-mix(in srgb, " <> c <> " 14%, var(--bg))")
              , ("--primary-ink", if isLight c then "#10201e" else "#ffffff")
              ]
            Nothing -> []
        , [("--font", fam) | Just k <- [get "theme_font"], k /= "nunito", (k', _, fam, _) <- fontFamilies, k' == k]
        , [("--bg", c) | Just c <- [get "theme_bg"]]
        , [("--page-bg", c) | Just c <- [get "theme_page"]]
        , [("--page-radius", r <> "px") | Just r <- [get "page_radius"]]
        ]
    siteVars =
      mapMaybe
        id
        [ (,) "--site-w" . px <$> get "site_width"
        , (,) "--site-pad" . px <$> get "site_pad"
        , (,) "--logo-h" . px <$> get "logo_h"
        ]
    footerVars =
      mapMaybe
        id
        [ (,) "--ft-bg" <$> get "footer_bg"
        , (,) "--ft-text" <$> get "footer_fg"
        , (,) "--ft-link" <$> get "footer_link"
        , (,) "--ft-w" . px <$> get "footer_width"
        , (,) "--ft-offset" . px <$> get "footer_offset"
        , (,) "--ft-pad" . px <$> get "footer_pad"
        , (,) "--ft-radius" . px <$> get "footer_radius"
        ]
    get k = M.lookup k s
    groupVars (p, pre) =
      mapMaybe
        id
        [ (,) (pre <> "-bg") <$> (rgba <$> get "nav_bg" <*> pure opacity)
        , (,) (pre <> "-text") <$> get "nav_text"
        , (,) (pre <> "-active") <$> get "nav_active"
        , (,) (pre <> "-pad") . px <$> get (p <> "_pad")
        , (,) (pre <> "-gap") . px <$> get (p <> "_gap")
        , (,) (pre <> "-offset") . px <$> get (p <> "_offset")
        , (,) (pre <> "-icon") . px <$> get (p <> "_icon")
        , (,) (pre <> "-radius") . px <$> get (p <> "_radius")
        , (,) (pre <> "-maxw") . px <$> get (p <> "_width")
        ]
          ++ fullVars p pre
    -- Lebar penuh: bar menempel ke atas dengan sudut tajam, kecuali jarak/sudut diatur sendiri.
    fullVars p pre
      | get (p <> "_full") == Just "1" =
          [(pre <> "-width", "100%"), (pre <> "-maxw", "none"), (pre <> "-track", "100%"), (pre <> "-side", "max(var(--site-pad, 20px), calc((100% - var(--site-w, 1140px)) / 2 + var(--site-pad, 20px)))")]
            ++ [(pre <> "-offset", "0px") | isNothing (get (p <> "_offset"))]
            ++ [(pre <> "-radius", "0px") | isNothing (get (p <> "_radius"))]
      | otherwise = []
    opacity = fromMaybe 90 (get "nav_opacity" >>= readInt)
    px v = v <> "px"

-- | "#0f766e" + 90 -> "rgba(15,118,110,0.9)"
rgba :: Text -> Int -> Text
rgba hex a =
  let h = T.drop 1 hex
      byte i = case readHex (T.unpack (T.take 2 (T.drop (i * 2) h))) of
        [(n, "")] -> n :: Int
        _ -> 0
      alpha
        | a >= 100 = "1"
        | otherwise = "0." <> T.justifyRight 2 '0' (T.pack (show a))
   in "rgba(" <> T.intercalate "," (map (T.pack . show . byte) [0, 1, 2]) <> "," <> alpha <> ")"

-- | Navigasi bawah aktif kecuali dimatikan dari page builder.
navBottomOn :: NavStyle -> Bool
navBottomOn s = M.lookup "bot_enabled" s /= Just "0"

-- | Gambar di situs bisa diklik untuk dibuka besar (popup), kecuali dimatikan.
lightboxOn :: NavStyle -> Bool
lightboxOn s = M.lookup "site_lightbox" s /= Just "0"

-- | Warna terang (luminans tinggi) butuh teks gelap di atasnya.
isLight :: Text -> Bool
isLight hex =
  let h = T.drop 1 hex
      byte i = case readHex (T.unpack (T.take 2 (T.drop (i * 2) h))) of
        [(n, "")] -> fromIntegral (n :: Int) :: Double
        _ -> 0
   in 0.299 * byte 0 + 0.587 * byte 1 + 0.114 * byte 2 > 170

-- | Pilihan huruf: (kunci, label, font-family CSS, parameter Google Fonts).
-- Harus sama dengan FONTS di static/builder.js.
fontFamilies :: [(Text, Text, Text, Text)]
fontFamilies =
  [ ("nunito", "Nunito (bawaan, ramah)", "", "")
  , ("inter", "Inter (modern, netral)", "\"Inter\", system-ui, sans-serif", "Inter:wght@400;500;600;700;800")
  , ("jakarta", "Plus Jakarta Sans (modern)", "\"Plus Jakarta Sans\", system-ui, sans-serif", "Plus+Jakarta+Sans:wght@400;500;600;700;800")
  , ("poppins", "Poppins (bulat, tegas)", "\"Poppins\", system-ui, sans-serif", "Poppins:wght@400;500;600;700;800")
  , ("montserrat", "Montserrat (elegan, lebar)", "\"Montserrat\", system-ui, sans-serif", "Montserrat:wght@400;500;600;700;800")
  , ("dmsans", "DM Sans (bersih)", "\"DM Sans\", system-ui, sans-serif", "DM+Sans:wght@400;500;600;700;800")
  , ("lora", "Lora (serif, klasik)", "\"Lora\", Georgia, serif", "Lora:wght@400;500;600;700")
  , ("playfair", "Playfair Display (serif, mewah)", "\"Playfair Display\", Georgia, serif", "Playfair+Display:wght@400;500;600;700;800")
  ]

-- | URL Google Fonts untuk huruf yang dipilih (Nunito sudah dimuat bawaan).
fontLink :: NavStyle -> Maybe Text
fontLink s = do
  k <- M.lookup "theme_font" s
  (_, _, _, q) <- lookupFont k
  if T.null q then Nothing else Just ("https://fonts.googleapis.com/css2?family=" <> q <> "&display=swap")
  where
    lookupFont k = case [f | f@(k', _, _, _) <- fontFamilies, k' == k] of
      (f : _) -> Just f
      [] -> Nothing

-- | Animasi muncul saat digulir (bawaan menyala).
revealOn :: NavStyle -> Bool
revealOn s = M.lookup "theme_reveal" s /= Just "0"

-- | Tombol kembali ke atas (bawaan menyala).
toTopOn :: NavStyle -> Bool
toTopOn s = M.lookup "theme_totop" s /= Just "0"

-- | Isian teks (Nothing bila kosong/belum diatur).
navText :: Text -> NavStyle -> Maybe Text
navText = M.lookup

-- | Pilihan nyala/mati dengan nilai bawaan.
navFlag :: Text -> Bool -> NavStyle -> Bool
navFlag k def s = maybe def (== "1") (M.lookup k s)

-- | Logo website (khusus situs). Kosong = memakai logo dashboard (menu Pengaturan).
siteLogo :: NavStyle -> Text -> Text
siteLogo s dashboardLogo = fromMaybe dashboardLogo (M.lookup "site_logo" s)

-- | Warna halaman diatur: isi halaman dibungkus kotak selebar website.
pageBoxOn :: NavStyle -> Bool
pageBoxOn = M.member "theme_page"
