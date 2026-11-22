-- | Penulis PDF minimal untuk laporan tabel: A4 lanskap, font standar Helvetica
-- (tanpa berkas font), judul tabel berulang di tiap halaman, teks panjang dibungkus,
-- dan nomor "Halaman x dari y".
module CMS.Pdf
  ( Report (..)
  , Column (..)
  , Tone (..)
  , pdfReport
  ) where

import qualified Codec.Compression.Zlib as Zlib
import Data.Array.Unboxed (UArray, listArray, (!))
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (showFFloat)

-- | Warna teks kolom penanda baris (mis. tingkat log).
data Tone = Normal | Alert | Severe

data Column = Column
  { colTitle :: Text
  , colWidth :: Double -- pt
  , colLines :: Int -- maks. baris per sel; sisanya dipotong dengan "…"
  }

data Report = Report
  { rpTitle :: Text
  , rpSubtitle :: Text
  , rpMeta :: [(Text, Text)] -- identitas laporan di halaman pertama
  , rpNote :: Text -- catatan kecil di bawah identitas
  , rpColumns :: [Column]
  , rpRows :: [(Tone, [Text])]
  , rpToneColumn :: Int -- kolom yang diwarnai sesuai Tone
  , rpFooter :: Text -- kiri bawah setiap halaman
  , rpAuthor :: Text
  , rpCreated :: Text -- tanggal PDF, mis. "D:20261122113745+07'00'"
  }

-- Tata letak -------------------------------------------------------------------------------

pageW, pageH, margin, fontSize, lineH, cellPad :: Double
pageW = 842
pageH = 595
margin = 36
fontSize = 7.5
lineH = 9.5
cellPad = 3.5

data Color = Color Double Double Double

ink, muted, accent, white, zebra, rule :: Color
ink = Color 0.13 0.16 0.16
muted = Color 0.42 0.45 0.5
accent = Color 0.06 0.46 0.43
white = Color 1 1 1
zebra = Color 0.96 0.975 0.97
rule = Color 0.86 0.88 0.88

toneColor :: Tone -> Color
toneColor = \case
  Normal -> ink
  Alert -> Color 0.66 0.4 0
  Severe -> Color 0.79 0.23 0.23

-- | Perintah gambar; y diukur dari atas halaman.
data Op
  = Txt Bool Double Color Double Double Text -- tebal?, ukuran, warna, x, y garis dasar, teks
  | Box Color Double Double Double Double -- warna, x, y atas, lebar, tinggi

-- | Halaman-halaman laporan (tanpa footer).
layout :: Report -> [[Op]]
layout Report {..} = go firstTop True rpRows
  where
    widths = map colWidth rpColumns
    xs = scanl (+) margin widths
    bottom = pageH - margin - 14
    -- Halaman pertama: judul, identitas, catatan.
    intro =
      [ Txt True 16 accent margin (margin + 14) rpTitle
      , Txt False 9 muted margin (margin + 28) rpSubtitle
      ]
        ++ concat (zipWith metaRow [0 ..] rpMeta)
        ++ [Txt False 7.5 muted margin (metaEnd + 12) l | l <- take 1 (wrap False 7.5 (pageW - 2 * margin) 1 rpNote)]
    metaTop = margin + 46
    metaEnd = metaTop + fromIntegral (length rpMeta) * 12
    metaRow :: Int -> (Text, Text) -> [Op]
    metaRow i (k, v) =
      let y = metaTop + fromIntegral i * 12
       in [Txt True 8.5 ink margin y k, Txt False 8.5 ink (margin + 120) y (T.concat (take 1 (wrap False 8.5 (pageW - 2 * margin - 120) 1 v)))]
    firstTop = metaEnd + 26
    otherTop = margin + 14
    running = [Txt False 7.5 muted margin margin (rpTitle <> " · " <> rpSubtitle)]
    header top =
      Box accent margin top (sum widths) 16
        : [Txt True fontSize white (x + cellPad) (top + 10.8) (colTitle col) | (x, col) <- zip xs rpColumns]
    go :: Double -> Bool -> [(Tone, [Text])] -> [[Op]]
    go top first rows = place (top + 16) (0 :: Int) rows ((if first then intro else running) ++ header top)
      where
        place _ _ [] page = [page]
        place y n ((tone, cells) : rest) page =
          let wrapped = zipWith (\col t -> wrap False fontSize (colWidth col - 2 * cellPad) (colLines col) t) rpColumns cells
              h = fromIntegral (maximum (1 : map length wrapped)) * lineH + 2 * cellPad - 2
           in if y + h > bottom && n > 0
                then page : go otherTop False ((tone, cells) : rest)
                else
                  let fill = [Box zebra margin y (sum widths) h | odd n]
                      sep = [Box rule margin (y + h - 0.4) (sum widths) 0.4]
                      texts =
                        [ Txt (i == rpToneColumn && isTone tone) fontSize (if i == rpToneColumn then toneColor tone else ink) (x + cellPad) (y + cellPad + 6 + fromIntegral j * lineH) l
                        | (i, x, ls) <- zip3 [0 ..] xs wrapped
                        , (j, l) <- zip [0 :: Int ..] ls
                        ]
                   in place (y + h) (n + 1) rest (page ++ fill ++ sep ++ texts)
    isTone = \case
      Normal -> False
      _ -> True

-- | Footer: keterangan di kiri, "Halaman x dari y" di kanan.
withFooters :: Text -> [[Op]] -> [[Op]]
withFooters note pages =
  [ page
      ++ [ Box rule margin (pageH - margin + 2) (pageW - 2 * margin) 0.4
         , Txt False 7 muted margin (pageH - margin + 12) note
         , let t = "Halaman " <> T.pack (show i) <> " dari " <> T.pack (show n)
            in Txt False 7 muted (pageW - margin - textWidth False 7 t) (pageH - margin + 12) t
         ]
  | (i, page) <- zip [1 :: Int ..] pages
  ]
  where
    n = length pages

-- Teks ------------------------------------------------------------------------------------

-- | Bungkus teks ke lebar tertentu (kata terlalu panjang dipecah per huruf); bila
-- melebihi @maxLines@, baris terakhir dipotong dengan "…".
wrap :: Bool -> Double -> Double -> Int -> Text -> [Text]
wrap bold size w maxLines t
  | length ls > maxLines = take (maxLines - 1) ls ++ [ellipsize (T.unwords (drop (maxLines - 1) ls))]
  | otherwise = ls
  where
    width = textWidth bold size
    -- Jumlah huruf awal yang muat dalam lebar @lim@.
    fitting lim s = length (takeWhile (<= lim) (drop 1 (scanl (+) 0 [charWidth bold ch * size / 1000 | ch <- T.unpack s])))
    ls = lineUp (concatMap split (T.words (clean t)))
    split word
      | width word <= w = [word]
      | otherwise = let (a, b) = T.splitAt (max 1 (fitting w word)) word in a : if T.null b then [] else split b
    lineUp [] = []
    lineUp (x : rest) = let (line, more) = grow x rest in line : lineUp more
    grow line (x : rest) | width (line <> " " <> x) <= w = grow (line <> " " <> x) rest
    grow line rest = (line, rest)
    ellipsize s = T.stripEnd (T.take (fitting (w - width "…") s) s) <> "…"

-- | Karakter di luar WinAnsi diganti padanan ASCII-nya.
clean :: Text -> Text
clean = T.replace "→" "->" . T.replace "←" "<-" . T.map (\ch -> if ch < ' ' then ' ' else ch)

textWidth :: Bool -> Double -> Text -> Double
textWidth bold size = (* (size / 1000)) . sum . map (charWidth bold) . T.unpack

-- | Lebar huruf Helvetica (AFM, per 1000 em) untuk ASCII; huruf lain ~ lebar "n".
charWidth :: Bool -> Char -> Double
charWidth bold ch
  | i >= 32 && i <= 126 = (if bold then boldW else regularW) ! i
  | otherwise = 556
  where
    i = ord ch

regularW, boldW :: UArray Int Double
regularW =
  listArray
    (32, 126)
    [ 278, 278, 355, 556, 556, 889, 667, 191, 333, 333, 389, 584, 278, 333, 278, 278
    , 556, 556, 556, 556, 556, 556, 556, 556, 556, 556, 278, 278, 584, 584, 584, 556
    , 1015, 667, 667, 722, 722, 667, 611, 778, 722, 278, 500, 667, 556, 833, 722, 778
    , 667, 778, 722, 667, 611, 722, 667, 944, 667, 667, 611, 278, 278, 278, 469, 556
    , 333, 556, 556, 500, 556, 556, 278, 556, 556, 222, 222, 500, 222, 833, 556, 556
    , 556, 556, 333, 500, 278, 556, 500, 722, 500, 500, 500, 334, 260, 334, 584
    ]
boldW =
  listArray
    (32, 126)
    [ 278, 333, 474, 556, 556, 889, 722, 238, 333, 333, 389, 584, 278, 333, 278, 278
    , 556, 556, 556, 556, 556, 556, 556, 556, 556, 556, 333, 333, 584, 584, 584, 611
    , 975, 722, 722, 722, 722, 667, 611, 778, 722, 278, 556, 722, 611, 833, 722, 778
    , 667, 778, 722, 667, 611, 722, 667, 944, 667, 667, 611, 333, 278, 333, 584, 556
    , 333, 556, 611, 556, 611, 556, 333, 611, 611, 278, 278, 556, 278, 889, 611, 611
    , 611, 611, 389, 556, 333, 611, 556, 778, 556, 556, 500, 389, 280, 389, 584
    ]

-- | Teks ke byte WinAnsi (untuk string PDF), dengan ( ) \ di-escape.
pdfString :: Text -> BB.Builder
pdfString t = BB.char7 '(' <> foldMap enc (T.unpack (clean t)) <> BB.char7 ')'
  where
    enc ch
      | ch `elem` ['(', ')', '\\'] = BB.char7 '\\' <> BB.char7 ch
      | ord ch < 128 = BB.char7 ch
      | ord ch >= 0xA0 && ord ch <= 0xFF = BB.word8 (fromIntegral (ord ch))
      | otherwise = BB.word8 $ case ch of
          '€' -> 0x80
          '…' -> 0x85
          '‘' -> 0x91
          '’' -> 0x92
          '“' -> 0x93
          '”' -> 0x94
          '•' -> 0x95
          '–' -> 0x96
          '—' -> 0x97
          _ -> 0x3F -- "?"

-- Berkas PDF ------------------------------------------------------------------------------

pdfReport :: Report -> BL.ByteString
pdfReport r = BB.toLazyByteString (header <> mconcat objs <> xref <> trailer)
  where
    pages = withFooters (rpFooter r) (layout r)
    nPages = length pages
    pageIds = [6 + 2 * i | i <- [0 .. nPages - 1]]
    objects =
      [ "<< /Type /Catalog /Pages 2 0 R >>"
      , "<< /Type /Pages /Count " <> int nPages <> " /Kids [" <> mconcat [int p <> " 0 R " | p <- pageIds] <> "] >>"
      , "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >>"
      , "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica-Bold /Encoding /WinAnsiEncoding >>"
      , "<< /Title " <> utf16 (rpTitle r <> " · " <> rpSubtitle r) <> " /Author " <> utf16 (rpAuthor r)
          <> " /Creator (DCMS) /Producer (DCMS) /CreationDate (" <> BB.byteString (TE.encodeUtf8 (rpCreated r)) <> ") >>"
      ]
        ++ concat
          [ [ "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 " <> num pageW <> " " <> num pageH <> "]"
                <> " /Resources << /Font << /F1 3 0 R /F2 4 0 R >> >> /Contents " <> int (p + 1) <> " 0 R >>"
            , stream (Zlib.compress (BB.toLazyByteString (foldMap draw page)))
            ]
          | (p, page) <- zip pageIds pages
          ]
    header = "%PDF-1.4\n%" <> foldMap BB.word8 [0xE2, 0xE3, 0xCF, 0xD3] <> "\n"
    rendered = zipWith (\i o -> BB.toLazyByteString (int i <> " 0 obj\n" <> o <> "\nendobj\n")) [1 :: Int ..] objects
    objs = map BB.lazyByteString rendered
    headerLen = BL.length (BB.toLazyByteString header)
    offsets = scanl (+) headerLen (map BL.length rendered)
    xref =
      "xref\n0 " <> int (length objects + 1) <> "\n0000000000 65535 f \n"
        <> mconcat [BB.string7 (pad10 o) <> " 00000 n \n" | o <- take (length objects) offsets]
    trailer =
      "trailer\n<< /Size " <> int (length objects + 1) <> " /Root 1 0 R /Info 5 0 R >>\nstartxref\n"
        <> BB.int64Dec (last offsets) <> "\n%%EOF\n"
    pad10 o = let s = show o in replicate (10 - length s) '0' ++ s
    stream body = "<< /Length " <> BB.int64Dec (BL.length body) <> " /Filter /FlateDecode >>\nstream\n" <> BB.lazyByteString body <> "\nendstream"
    utf16 t = "<FEFF" <> BB.byteString (B16.encode (TE.encodeUtf16BE t)) <> ">"

draw :: Op -> BB.Builder
draw = \case
  Box c x y w h -> color c <> " rg " <> num x <> " " <> num (pageH - y - h) <> " " <> num w <> " " <> num h <> " re f\n"
  Txt bold size c x y t ->
    "BT " <> (if bold then "/F2 " else "/F1 ") <> num size <> " Tf " <> color c <> " rg "
      <> num x <> " " <> num (pageH - y) <> " Td " <> pdfString t <> " Tj ET\n"
  where
    color (Color r g b) = num r <> " " <> num g <> " " <> num b

num :: Double -> BB.Builder
num d = BB.string7 (showFFloat (Just 2) d "")

int :: Int -> BB.Builder
int = BB.intDec
