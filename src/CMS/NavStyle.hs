-- | Pengaturan warna & jarak navigasi atas (header) dan navigasi bawah (tab bar).
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
  ) where

import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isHexDigit)
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
  [ NavGroup "theme" "Gaya situs" "fa-solid fa-palette"
      [ NavField "theme_primary" "Warna utama (tombol, tautan, aksen)" NavColor
      , NavField "theme_font" "Jenis huruf" (NavSelect "nunito" [(k, l) | (k, l, _, _) <- fontFamilies])
      , NavField "theme_reveal" "Animasi muncul saat digulir" (NavToggle True)
      , NavField "theme_totop" "Tombol kembali ke atas" (NavToggle True)
      ]
  , NavGroup "site" "Ukuran website" "fa-solid fa-arrows-left-right-to-line"
      [ NavField "site_width" "Lebar maksimal konten" (NavRange 720 1920 1140 "px")
      , NavField "site_pad" "Jarak tepi kiri-kanan" (NavRange 8 80 20 "px")
      , NavField "site_lightbox" "Popup gambar saat diklik" (NavToggle True)
      ]
  , NavGroup "top" "Navigasi atas" "fa-solid fa-window-maximize" $
      [NavField "top_full" "Lebar penuh (dari tepi ke tepi)" (NavToggle False)]
        ++ fields "top" "Jarak dari atas" 48 12 32 9 32 2
        ++ [NavField "top_radius" "Kelengkungan sudut" (NavRange 0 40 22 "px")]
  , NavGroup "bot" "Navigasi bawah" "fa-solid fa-table-columns" $
      [NavField "bot_enabled" "Tampilkan navigasi bawah" (NavToggle True)]
        ++ fields "bot" "Jarak dari bawah" 60 14 24 5 24 2
        ++ [NavField "bot_icon" "Ukuran ikon" (NavRange 10 28 18 "px")]
  ]
  where
    fields p offLabel offMax offDef padMax padDef gapMax gapDef =
      [ NavField (p <> "_bg") "Warna latar" NavColor
      , NavField (p <> "_opacity") "Kepekatan latar" (NavRange 0 100 90 "%")
      , NavField (p <> "_text") "Warna teks menu" NavColor
      , NavField (p <> "_active") "Warna menu aktif" NavColor
      , NavField (p <> "_pad") "Jarak dalam (tinggi bar)" (NavRange 0 padMax padDef "px")
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

cleanColor :: Text -> Maybe Text
cleanColor v = case T.uncons (T.toLower v) of
  Just ('#', h) | T.length h == 6 && T.all isHexDigit h -> Just ("#" <> T.toLower h)
  _ -> Nothing

decodeNav :: Text -> NavStyle
decodeNav = sanitizeNav . fromMaybe M.empty . decodeStrict . TE.encodeUtf8

encodeNav :: NavStyle -> Text
encodeNav = TE.decodeUtf8 . BL.toStrict . encode

-- | Isi elemen <style>: variabel CSS untuk nilai yang diisi saja. Logika yang sama
-- dipakai static/builder.js untuk pratinjau langsung.
navCss :: NavStyle -> Text
navCss s
  | null vars = ""
  | otherwise = ":root{" <> T.concat [k <> ":" <> v <> ";" | (k, v) <- vars] <> "}"
  where
    vars = themeVars ++ siteVars ++ concatMap groupVars [("top", "--nt"), ("bot", "--nb")]
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
        ]
    siteVars =
      mapMaybe
        id
        [ (,) "--site-w" . px <$> get "site_width"
        , (,) "--site-pad" . px <$> get "site_pad"
        ]
    get k = M.lookup k s
    groupVars (p, pre) =
      mapMaybe
        id
        [ (,) (pre <> "-bg") <$> (rgba <$> get (p <> "_bg") <*> pure (opacity p))
        , (,) (pre <> "-text") <$> get (p <> "_text")
        , (,) (pre <> "-active") <$> get (p <> "_active")
        , (,) (pre <> "-pad") . px <$> get (p <> "_pad")
        , (,) (pre <> "-gap") . px <$> get (p <> "_gap")
        , (,) (pre <> "-offset") . px <$> get (p <> "_offset")
        , (,) (pre <> "-icon") . px <$> get (p <> "_icon")
        , (,) (pre <> "-radius") . px <$> get (p <> "_radius")
        ]
          ++ fullVars p pre
    -- Lebar penuh: bar menempel ke atas dengan sudut tajam, kecuali jarak/sudut diatur sendiri.
    fullVars p pre
      | get (p <> "_full") == Just "1" =
          [(pre <> "-width", "100%"), (pre <> "-maxw", "none"), (pre <> "-side", "max(var(--site-pad, 20px), calc((100% - var(--site-w, 1140px)) / 2 + var(--site-pad, 20px)))")]
            ++ [(pre <> "-offset", "0px") | isNothing (get (p <> "_offset"))]
            ++ [(pre <> "-radius", "0px") | isNothing (get (p <> "_radius"))]
      | otherwise = []
    opacity p = fromMaybe 90 (get (p <> "_opacity") >>= readInt)
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
