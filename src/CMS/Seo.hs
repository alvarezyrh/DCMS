-- | Analisis SEO artikel: daftar periksa + skor 0–100.
--
-- Aturan yang sama dipakai static/app.js (seoAnalyze) untuk skor langsung di editor,
-- jadi bila mengubah aturan di sini, ubah juga di sana.
module CMS.Seo
  ( Level (..)
  , Check (..)
  , SeoInput (..)
  , seoInputOf
  , analyze
  , seoScore
  , scoreLevel
  , levelLabel
  , levelClass
  , effectiveTitle
  , effectiveDesc
  , firstImage
  , countWords
  ) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

import CMS.Types
import CMS.Util (isBlank, plainExcerpt, slugify)

data Level = Good | Warn | Bad
  deriving (Eq, Show)

data Check = Check
  { ckLevel :: Level
  , ckText :: Text
  }

data SeoInput = SeoInput
  { siTitle :: Text -- judul artikel
  , siSlug :: Text
  , siBody :: Text -- Markdown
  , siExcerpt :: Text
  , siSeo :: PostSeo
  }

seoInputOf :: Post -> SeoInput
seoInputOf p = SeoInput (postTitle p) (postSlug p) (postBody p) (postExcerpt p) (postSeo p)

-- | Judul di hasil pencarian: judul SEO, atau judul artikel.
effectiveTitle :: Text -> PostSeo -> Text
effectiveTitle title s = if isBlank (seoTitle s) then title else T.strip (seoTitle s)

-- | Deskripsi di hasil pencarian: deskripsi meta, ringkasan, atau awal isi artikel.
effectiveDesc :: Text -> Text -> PostSeo -> Text
effectiveDesc excerpt body s
  | not (isBlank (seoDesc s)) = T.strip (seoDesc s)
  | not (isBlank excerpt) = T.strip excerpt
  | otherwise = plainExcerpt 160 body

-- | URL gambar Markdown pertama: ![alt](url).
firstImage :: Text -> Maybe Text
firstImage body = case filter safe (map snd (images body)) of
  (u : _) -> Just u
  [] -> Nothing
  where
    -- Hanya tautan http(s) atau path situs ini (bukan javascript:, data:, //host).
    safe u =
      ("/" `T.isPrefixOf` u && not ("//" `T.isPrefixOf` u))
        || any (`T.isPrefixOf` T.toLower u) ["https://", "http://"]

-- | Semua gambar Markdown: (alt, url).
images :: Text -> [(Text, Text)]
images t = case T.breakOn "![" t of
  (_, rest) | T.null rest -> []
  (_, rest) ->
    let afterBang = T.drop 2 rest
        (alt, r1) = T.breakOn "](" afterBang
        (url, r2) = T.breakOn ")" (T.drop 2 r1)
     in if T.null r1 || T.null r2 || T.any (== '\n') alt
          then images afterBang
          else (T.strip alt, T.strip (T.takeWhile (/= ' ') (T.strip url))) : images (T.drop 1 r2)

-- | Tujuan tautan Markdown [teks](url), tanpa gambar.
links :: Text -> [Text]
links t = filter (`notElem` map snd (images t)) (targets t)
  where
    targets x = case T.breakOn "](" x of
      (_, rest)
        | T.null rest -> []
        | otherwise ->
            let (url, r2) = T.breakOn ")" (T.drop 2 rest)
             in if T.null r2 then [] else T.strip (T.takeWhile (/= ' ') (T.strip url)) : targets (T.drop 1 r2)

countWords :: Text -> Int
countWords = length . filter (T.any isAlphaNum) . T.words

lower :: Text -> Text
lower = T.toLower . T.strip

-- | Paragraf pertama yang berisi teks (bukan judul, gambar, atau kode).
firstParagraph :: Text -> Text
firstParagraph body =
  case filter isText (T.splitOn "\n\n" (T.replace "\r" "" body)) of
    (p : _) -> p
    [] -> ""
  where
    isText p =
      let s = T.strip p
       in not (T.null s) && not ("#" `T.isPrefixOf` s) && not ("![" `T.isPrefixOf` s) && not ("```" `T.isPrefixOf` s)

-- | Jumlah kemunculan frasa kata kunci (tanpa membedakan huruf besar/kecil).
occurrences :: Text -> Text -> Int
occurrences kw t
  | T.null kw = 0
  | otherwise = length (T.breakOnAll kw t)

analyze :: SeoInput -> [Check]
analyze SeoInput {..} =
  [ if hasKw then Check Good ("Kata kunci utama: “" <> T.strip (seoKeyword siSeo) <> "”") else Check Bad "Tentukan kata kunci utama artikel ini"
  , kwCheck (kw `T.isInfixOf` lower title) "Kata kunci ada di judul SEO" "Masukkan kata kunci ke judul SEO"
  , kwCheck (kw `T.isInfixOf` lower desc) "Kata kunci ada di deskripsi meta" "Masukkan kata kunci ke deskripsi meta"
  , kwCheck (slugify kw `T.isInfixOf` siSlug) "Kata kunci ada di URL (slug)" "Masukkan kata kunci ke URL (slug)"
  , kwCheck (kw `T.isInfixOf` lower (firstParagraph siBody)) "Kata kunci muncul di paragraf pertama" "Sebutkan kata kunci di paragraf pertama"
  , if
      | not hasKw -> Check Bad "Kepadatan kata kunci belum bisa dihitung"
      | density < 0.5 -> Check Warn ("Kata kunci jarang dipakai (" <> pct density <> "); idealnya 0,5–2,5%")
      | density > 2.5 -> Check Warn ("Kata kunci terlalu sering (" <> pct density <> "); idealnya 0,5–2,5%")
      | otherwise -> Check Good ("Kepadatan kata kunci pas (" <> pct density <> ")")
  , let n = T.length title
     in if
          | n >= 30 && n <= 60 -> Check Good ("Panjang judul SEO pas (" <> tshow' n <> " karakter)")
          | n == 0 -> Check Bad "Judul masih kosong"
          | n < 30 -> Check Warn ("Judul SEO terlalu pendek (" <> tshow' n <> "/30–60 karakter)")
          | otherwise -> Check Warn ("Judul SEO terlalu panjang, akan terpotong di Google (" <> tshow' n <> "/60)")
  , let n = T.length desc
     in if
          | isBlank (seoDesc siSeo) -> Check Bad "Tulis deskripsi meta sendiri (saat ini diambil otomatis)"
          | n >= 120 && n <= 160 -> Check Good ("Panjang deskripsi meta pas (" <> tshow' n <> " karakter)")
          | n < 120 -> Check Warn ("Deskripsi meta terlalu pendek (" <> tshow' n <> "/120–160 karakter)")
          | otherwise -> Check Warn ("Deskripsi meta terlalu panjang (" <> tshow' n <> "/160 karakter)")
  , if
      | wc >= 300 -> Check Good ("Panjang artikel cukup (" <> tshow' wc <> " kata)")
      | wc >= 150 -> Check Warn ("Artikel agak pendek (" <> tshow' wc <> " kata); targetkan 300+")
      | otherwise -> Check Bad ("Artikel terlalu pendek (" <> tshow' wc <> " kata); targetkan 300+")
  , if any (\l -> "## " `T.isPrefixOf` l || "### " `T.isPrefixOf` l) (map T.strip (T.lines siBody))
      then Check Good "Ada subjudul (## …) untuk memecah isi"
      else Check Warn "Tambahkan subjudul (## …) agar mudah dibaca"
  , case images siBody of
      [] -> Check Warn "Tambahkan minimal satu gambar"
      imgs
        | any (T.null . fst) imgs -> Check Bad "Ada gambar tanpa teks alternatif (alt)"
        | otherwise -> Check Good "Semua gambar punya teks alternatif"
  , if any internal (links siBody)
      then Check Good "Ada tautan ke halaman lain di situs ini"
      else Check Warn "Tambahkan tautan ke artikel/halaman lain di situs ini"
  ]
  where
    kw = lower (seoKeyword siSeo)
    hasKw = not (T.null kw)
    title = effectiveTitle siTitle siSeo
    desc = effectiveDesc siExcerpt siBody siSeo
    wc = countWords siBody
    density = if wc == 0 then 0 else fromIntegral (occurrences kw (T.toLower siBody)) * 100 / fromIntegral wc :: Double
    kwCheck ok good bad
      | not hasKw = Check Bad bad
      | ok = Check Good good
      | otherwise = Check Bad bad
    internal u = "/" `T.isPrefixOf` u && not ("//" `T.isPrefixOf` u)
    pct d = T.replace "." "," (T.pack (show (fromIntegral (round (d * 10) :: Int) / 10 :: Double))) <> "%"
    tshow' = T.pack . show

-- | Skor 0–100: Baik = 1, Cukup = ½, Buruk = 0, dirata-rata.
seoScore :: [Check] -> Int
seoScore [] = 0
seoScore cs = round (100 * sum (map w cs) / fromIntegral (length cs) :: Double)
  where
    w c = case ckLevel c of
      Good -> 1
      Warn -> 0.5
      Bad -> 0

scoreLevel :: Int -> Level
scoreLevel n
  | n >= 80 = Good
  | n >= 50 = Warn
  | otherwise = Bad

levelLabel :: Level -> Text
levelLabel = \case
  Good -> "Baik"
  Warn -> "Cukup"
  Bad -> "Perlu perbaikan"

levelClass :: Level -> Text
levelClass = \case
  Good -> "lv-good"
  Warn -> "lv-warn"
  Bad -> "lv-bad"
