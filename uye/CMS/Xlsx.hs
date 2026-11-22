-- | Penulis buku kerja Excel (.xlsx) minimal: beberapa lembar, teks & angka, beberapa gaya.
module CMS.Xlsx
  ( Cell (..)
  , Style (..)
  , Sheet (..)
  , DocInfo (..)
  , xlsxWorkbook
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import CMS.Util (tshow)
import CMS.Zip (zipArchive)

data Style = Plain | Bold | Title | Header | Muted | Red | Amber
  deriving (Eq, Enum, Bounded)

data Cell = Txt Style Text | Num Style Int | Blank

data Sheet = Sheet
  { shName :: Text
  , shWidths :: [Double] -- lebar kolom (satuan karakter Excel)
  , shRows :: [[Cell]]
  , shTable :: Bool -- baris pertama = judul tabel: dibekukan & diberi filter
  }

-- | Properti dokumen (File › Info di Excel).
data DocInfo = DocInfo
  { diTitle :: Text
  , diSubject :: Text
  , diCreator :: Text
  , diCreated :: Text -- ISO 8601 UTC, mis. "2026-11-22T04:37:45Z"
  }

xlsxWorkbook :: DocInfo -> [Sheet] -> BL.ByteString
xlsxWorkbook info sheets =
  zipArchive $
    [ ("[Content_Types].xml", xml (contentTypes n))
    , ("_rels/.rels", rootRels)
    , ("docProps/core.xml", xml (coreProps info))
    , ("xl/workbook.xml", xml (workbook (map shName sheets)))
    , ("xl/_rels/workbook.xml.rels", xml (workbookRels n))
    , ("xl/styles.xml", styles)
    ]
      ++ [("xl/worksheets/sheet" <> tshow i <> ".xml", xml (worksheet s)) | (i, s) <- zip [1 :: Int ..] sheets]
  where
    n = length sheets
    xml = TLE.encodeUtf8 . TL.fromStrict

worksheet :: Sheet -> Text
worksheet Sheet {..} =
  T.concat $
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
    , "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">"
    , "<sheetPr><pageSetUpPr fitToPage=\"1\"/></sheetPr>"
    , "<sheetViews><sheetView workbookViewId=\"0\">"
    , if shTable then "<pane ySplit=\"1\" topLeftCell=\"A2\" activePane=\"bottomLeft\" state=\"frozen\"/>" else ""
    , "</sheetView></sheetViews>"
    , "<cols>"
    ]
      ++ [ "<col min=\"" <> tshow i <> "\" max=\"" <> tshow i <> "\" width=\"" <> tshow w <> "\" customWidth=\"1\"/>"
         | (i, w) <- zip [1 :: Int ..] shWidths
         ]
      ++ ["</cols><sheetData>"]
      ++ zipWith row [1 ..] shRows
      ++ ["</sheetData>"]
      ++ ["<autoFilter ref=\"A1:" <> colName (length shWidths) <> tshow (length shRows) <> "\"/>" | shTable, length shRows > 1]
      ++ ["<pageSetup orientation=\"landscape\" paperSize=\"9\" fitToWidth=\"1\" fitToHeight=\"0\"/>", "</worksheet>"]
  where
    row :: Int -> [Cell] -> Text
    row r cells = "<row r=\"" <> tshow r <> "\">" <> T.concat (zipWith (cell r) [1 ..] cells) <> "</row>"
    cell r col = \case
      Blank -> ""
      Num st n -> "<c r=\"" <> ref r col <> "\"" <> styleAttr st <> "><v>" <> tshow n <> "</v></c>"
      Txt st t -> "<c r=\"" <> ref r col <> "\" t=\"inlineStr\"" <> styleAttr st <> "><is><t xml:space=\"preserve\">" <> esc t <> "</t></is></c>"
    ref r col = colName col <> tshow r
    styleAttr st = if st == Plain then "" else " s=\"" <> tshow (fromEnum st) <> "\""

-- | 1 -> "A", 27 -> "AA".
colName :: Int -> Text
colName n
  | n <= 0 = ""
  | otherwise = colName ((n - 1) `div` 26) <> T.singleton (chr (ord 'A' + (n - 1) `mod` 26))

-- | Escape XML dan buang karakter kontrol yang tidak sah di XML 1.0.
esc :: Text -> Text
esc = T.concatMap $ \case
  '<' -> "&lt;"
  '>' -> "&gt;"
  '&' -> "&amp;"
  '"' -> "&quot;"
  ch | ch < ' ' && ch `notElem` ['\t', '\n'] -> ""
  ch -> T.singleton ch

-- | Urutan <xf> mengikuti urutan konstruktor Style (indeks = fromEnum).
styles :: BL.ByteString
styles =
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\
  \<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">\
  \<fonts count=\"7\">\
  \<font><sz val=\"10\"/><name val=\"Calibri\"/></font>\
  \<font><b/><sz val=\"10\"/><name val=\"Calibri\"/></font>\
  \<font><b/><sz val=\"15\"/><color rgb=\"FF0F766E\"/><name val=\"Calibri\"/></font>\
  \<font><b/><sz val=\"10\"/><color rgb=\"FFFFFFFF\"/><name val=\"Calibri\"/></font>\
  \<font><sz val=\"9\"/><color rgb=\"FF6B7280\"/><name val=\"Calibri\"/></font>\
  \<font><b/><sz val=\"10\"/><color rgb=\"FFC93A3A\"/><name val=\"Calibri\"/></font>\
  \<font><b/><sz val=\"10\"/><color rgb=\"FFA86500\"/><name val=\"Calibri\"/></font>\
  \</fonts>\
  \<fills count=\"3\">\
  \<fill><patternFill patternType=\"none\"/></fill>\
  \<fill><patternFill patternType=\"gray125\"/></fill>\
  \<fill><patternFill patternType=\"solid\"><fgColor rgb=\"FF0F766E\"/></patternFill></fill>\
  \</fills>\
  \<borders count=\"1\"><border/></borders>\
  \<cellStyleXfs count=\"1\"><xf/></cellStyleXfs>\
  \<cellXfs count=\"7\">\
  \<xf fontId=\"0\" applyAlignment=\"1\"><alignment vertical=\"top\"/></xf>\
  \<xf fontId=\"1\" applyFont=\"1\" applyAlignment=\"1\"><alignment vertical=\"top\"/></xf>\
  \<xf fontId=\"2\" applyFont=\"1\"/>\
  \<xf fontId=\"3\" fillId=\"2\" applyFont=\"1\" applyFill=\"1\"/>\
  \<xf fontId=\"4\" applyFont=\"1\" applyAlignment=\"1\"><alignment vertical=\"top\"/></xf>\
  \<xf fontId=\"5\" applyFont=\"1\" applyAlignment=\"1\"><alignment vertical=\"top\"/></xf>\
  \<xf fontId=\"6\" applyFont=\"1\" applyAlignment=\"1\"><alignment vertical=\"top\"/></xf>\
  \</cellXfs>\
  \<cellStyles count=\"1\"><cellStyle name=\"Normal\" xfId=\"0\" builtinId=\"0\"/></cellStyles>\
  \</styleSheet>"

contentTypes :: Int -> Text
contentTypes n =
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\
  \<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">\
  \<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>\
  \<Default Extension=\"xml\" ContentType=\"application/xml\"/>\
  \<Override PartName=\"/xl/workbook.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml\"/>\
  \<Override PartName=\"/xl/styles.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml\"/>\
  \<Override PartName=\"/docProps/core.xml\" ContentType=\"application/vnd.openxmlformats-package.core-properties+xml\"/>"
    <> T.concat
      [ "<Override PartName=\"/xl/worksheets/sheet" <> tshow i <> ".xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>"
      | i <- [1 .. n]
      ]
    <> "</Types>"

rootRels :: BL.ByteString
rootRels =
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\
  \<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">\
  \<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"xl/workbook.xml\"/>\
  \<Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties\" Target=\"docProps/core.xml\"/>\
  \</Relationships>"

-- | rId1..rIdN = lembar, rIdN+1 = gaya.
workbookRels :: Int -> Text
workbookRels n =
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\
  \<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
    <> T.concat
      [ "<Relationship Id=\"rId" <> tshow i <> "\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\" Target=\"worksheets/sheet" <> tshow i <> ".xml\"/>"
      | i <- [1 .. n]
      ]
    <> "<Relationship Id=\"rId" <> tshow (n + 1) <> "\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles\" Target=\"styles.xml\"/>\
       \</Relationships>"

workbook :: [Text] -> Text
workbook names =
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\
  \<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" \
  \xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"><sheets>"
    <> T.concat
      [ "<sheet name=\"" <> esc (T.take 31 nm) <> "\" sheetId=\"" <> tshow i <> "\" r:id=\"rId" <> tshow i <> "\"/>"
      | (i, nm) <- zip [1 :: Int ..] names
      ]
    <> "</sheets></workbook>"

coreProps :: DocInfo -> Text
coreProps DocInfo {..} =
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\
  \<cp:coreProperties xmlns:cp=\"http://schemas.openxmlformats.org/package/2006/metadata/core-properties\" \
  \xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:dcterms=\"http://purl.org/dc/terms/\" \
  \xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">"
    <> "<dc:title>" <> esc diTitle <> "</dc:title>"
    <> "<dc:subject>" <> esc diSubject <> "</dc:subject>"
    <> "<dc:creator>" <> esc diCreator <> "</dc:creator>"
    <> "<dcterms:created xsi:type=\"dcterms:W3CDTF\">" <> esc diCreated <> "</dcterms:created>"
    <> "</cp:coreProperties>"
