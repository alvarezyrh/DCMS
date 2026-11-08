-- | Fungsi bantu murni: slug, tanggal, markdown, parsing angka.
module CMS.Util where

import CMark (commonmarkToHtml, optSmart)
import Data.Char (isAlphaNum, isAscii, isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Read (readMaybe)

slugify :: Text -> Text
slugify t =
  let mapped = T.map (\c -> if isAscii c && isAlphaNum c then c else '-') (T.toLower t)
      parts = filter (not . T.null) (T.splitOn "-" mapped)
      s = T.take 80 (T.intercalate "-" parts)
   in if T.null s then "item" else s

-- | Cari slug unik: slug, slug-2, slug-3, ...
uniqueSlug :: (Text -> IO Bool) -> Text -> IO Text
uniqueSlug taken base = go (1 :: Int)
  where
    go n = do
      let cand = if n == 1 then base else base <> "-" <> T.pack (show n)
      t <- taken cand
      if t then go (n + 1) else pure cand

readInt :: Text -> Maybe Int
readInt t = case TR.decimal t of
  Right (n, rest) | T.null rest -> Just n
  _ -> Nothing

-- | "2026-11-06 10:00:00" -> "6 Nov 2026"
fmtDate :: Text -> Text
fmtDate t = case T.splitOn "-" (T.take 10 t) of
  [y, m, d]
    | Just mi <- readInt m
    , mi >= 1 && mi <= 12
    , Just di <- readInt d ->
        T.pack (show di) <> " " <> (months !! (mi - 1)) <> " " <> y
  _ -> t
  where
    months = ["Jan", "Feb", "Mar", "Apr", "Mei", "Jun", "Jul", "Agu", "Sep", "Okt", "Nov", "Des"]

-- | Markdown ke HTML. cmark >= 0.6 membuang HTML mentah secara default.
renderMarkdown :: Text -> Text
renderMarkdown = commonmarkToHtml [optSmart]

-- | Ringkasan teks polos dari markdown.
plainExcerpt :: Int -> Text -> Text
plainExcerpt n body =
  let plain = T.unwords (T.words (T.filter (`notElem` ("#*_`>[]" :: String)) body))
   in if T.length plain > n then T.stripEnd (T.take n plain) <> "…" else plain

nonEmpty :: Text -> Maybe Text
nonEmpty t = if T.null t then Nothing else Just t

totalPages :: Int -> Int -> Int
totalPages total per = max 1 ((total + per - 1) `div` per)

isBlank :: Text -> Bool
isBlank = T.all isSpace

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Nomor Indonesia -> format internasional tanpa "+", mis. "0822-9999-5125" -> "6282299995125".
waNumber :: Text -> Text
waNumber t =
  let d = T.filter isDigit t
   in if "0" `T.isPrefixOf` d then "62" <> T.drop 1 d else d

-- | Tampilan nomor yang seragam: "0822-9999-5125".
fmtPhone :: Text -> Text
fmtPhone t =
  let d0 = T.filter isDigit t
      d = if "62" `T.isPrefixOf` d0 then "0" <> T.drop 2 d0 else d0
   in if T.length d < 9 then T.strip t else T.intercalate "-" [T.take 4 d, T.take 4 (T.drop 4 d), T.drop 8 d]

-- | "-6.2088, 106.8456" -> Just (-6.2088, 106.8456), dalam rentang yang sah.
parseCoords :: Text -> Maybe (Double, Double)
parseCoords t = case map T.strip (T.splitOn "," t) of
  [a, b] -> do
    la <- readMaybe (T.unpack a)
    lo <- readMaybe (T.unpack b)
    if abs la <= 90 && abs lo <= 180 then Just (la, lo) else Nothing
  _ -> Nothing

-- | Jarak garis lurus dua titik (km), rumus haversine.
haversineKm :: (Double, Double) -> (Double, Double) -> Double
haversineKm (la1, lo1) (la2, lo2) =
  let r = (* (pi / 180))
      dLat = r (la2 - la1)
      dLng = r (lo2 - lo1)
      x = sin (dLat / 2) ^ (2 :: Int) + cos (r la1) * cos (r la2) * sin (dLng / 2) ^ (2 :: Int)
   in 2 * 6371 * asin (sqrt x)
