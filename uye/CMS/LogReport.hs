-- | Laporan log keamanan untuk diunduh (Excel .xlsx atau PDF). Setiap laporan punya
-- nomor dan sidik SHA-256 dari seluruh catatannya; unduhan juga dicatat di log beserta
-- nomor & sidik itu, sehingga keaslian laporan bisa dicocokkan kemudian.
module CMS.LogReport
  ( ReportFormat (..)
  , formatByKey
  , formatLabel
  , LogReport (..)
  , exportLimit
  , buildReport
  , reportFile
  ) where

import Crypto.Hash (SHA256 (..), hashWith)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time

import CMS.Log
import CMS.Pdf (Column (..), Report (..), Tone (..), pdfReport)
import CMS.Types
import CMS.Util (fmtDateTime, fmtInt, tshow)
import CMS.Xlsx (Cell (..), DocInfo (..), Sheet (..), Style (..), xlsxWorkbook)

data ReportFormat = Xlsx | Pdf

formatByKey :: Text -> Maybe ReportFormat
formatByKey = \case
  "xlsx" -> Just Xlsx
  "pdf" -> Just Pdf
  _ -> Nothing

formatLabel :: ReportFormat -> Text
formatLabel = \case
  Xlsx -> "Excel"
  Pdf -> "PDF"

-- | Batas catatan per laporan (yang terbaru).
exportLimit :: Int
exportLimit = 10000

data LogReport = LogReport
  { lrNumber :: Text -- mis. "LOG-20261122-113745-3FA9C1"
  , lrDigest :: Text -- SHA-256 (heksadesimal) seluruh catatan
  , lrSite :: Text
  , lrBy :: Text
  , lrAt :: ZonedTime
  , lrFilter :: LogFilter
  , lrRows :: [LogEntry]
  , lrTotal :: Int -- jumlah catatan yang cocok (bisa lebih dari lrRows bila dibatasi)
  }

buildReport :: Text -> User -> ZonedTime -> LogFilter -> [LogEntry] -> Int -> LogReport
buildReport site u now f rows total =
  LogReport
    { lrNumber = "LOG-" <> stamp now <> "-" <> T.toUpper (T.take 6 digest)
    , lrDigest = digest
    , lrSite = site
    , lrBy = userShownName u <> " (@" <> userName u <> ")"
    , lrAt = now
    , lrFilter = f
    , lrRows = rows
    , lrTotal = total
    }
  where
    digest = T.pack (show (hashWith SHA256 (TE.encodeUtf8 (canonical rows))))

-- | Isi yang di-hash: satu baris per catatan, kolom dipisah tab, sesuai urutan laporan.
canonical :: [LogEntry] -> Text
canonical rows =
  T.unlines
    [ T.intercalate "\t" [tshow (leId e), leAt e, leLevel e, leEvent e, leIp e, leActor e, leAccount e, leMethod e, lePath e, leAgent e, leDetail e]
    | e <- rows
    ]

-- | (nama berkas, Content-Type, isi)
reportFile :: ReportFormat -> LogReport -> (Text, ByteString, BL.ByteString)
reportFile fmt r = case fmt of
  Xlsx -> (base <> ".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", xlsx r)
  Pdf -> (base <> ".pdf", "application/pdf", pdf r)
  where
    base = "laporan-log-" <> stamp (lrAt r)

-- Isi laporan -----------------------------------------------------------------------------

title :: Text
title = "Laporan Log Keamanan"

-- | Identitas laporan (sama di Excel dan PDF).
meta :: LogReport -> [(Text, Text)]
meta LogReport {..} =
  [ ("Nomor laporan", lrNumber)
  , ("Situs", lrSite)
  , ("Periode", period lrAt (lfDays lrFilter))
  , ("Filter", filterText lrFilter)
  , ("Jumlah catatan", count)
  , ("Ringkasan", T.intercalate " · " [levelLabel l <> " " <> fmtInt (length [e | e <- lrRows, leLevel e == levelKey l]) | l <- [Danger, Warn, Info]])
  , ("Dibuat", fmtDateTime (T.pack (formatTime defaultTimeLocale "%F %T" lrAt)) <> " " <> T.pack (formatTime defaultTimeLocale "%Z" lrAt))
  , ("Dibuat oleh", lrBy)
  , ("Sidik SHA-256", lrDigest)
  ]
  where
    shown = length lrRows
    count
      | lrTotal > shown = fmtInt shown <> " terbaru dari " <> fmtInt lrTotal <> " (dibatasi " <> fmtInt exportLimit <> " per laporan)"
      | otherwise = fmtInt shown

note :: Text
note =
  "Sidik SHA-256 dihitung dari seluruh catatan di laporan ini (isi dan urutannya). Unduhan ini tercatat di menu Log \
  \sebagai \"Laporan diunduh\" beserta nomor dan sidiknya, sehingga keaslian laporan dapat dicocokkan."

period :: ZonedTime -> Int -> Text
period now days
  | days <= 0 = "Semua catatan tersimpan (maks. 180 hari)"
  | otherwise =
      let from = utcToZonedTime (zonedTimeZone now) (addUTCTime (fromIntegral (negate days * 86400)) (zonedTimeToUTC now))
          label = if days == 1 then "24 jam terakhir" else tshow days <> " hari terakhir"
       in label <> " (" <> clock from <> " – " <> clock now <> ")"
  where
    clock = fmtDateTime . T.pack . formatTime defaultTimeLocale "%F %T"

filterText :: LogFilter -> Text
filterText LogFilter {..} =
  case parts of
    [] -> "Tanpa filter"
    _ -> T.intercalate "; " parts
  where
    parts =
      [ "Tingkat: " <> lvl | Just lvl <- [lookup lfLevel levels] ]
        ++ ["Peristiwa: " <> maybe lfEvent eventLabel (eventByKey lfEvent) | not (T.null lfEvent)]
        ++ ["Pencarian: \"" <> lfSearch <> "\"" | not (T.null lfSearch)]
    levels = ("alert", "Peringatan & bahaya") : [(levelKey l, levelLabel l) | l <- [minBound .. maxBound]]

-- | "20261122-113745"
stamp :: ZonedTime -> Text
stamp = T.pack . formatTime defaultTimeLocale "%Y%m%d-%H%M%S"

eventText :: LogEntry -> Text
eventText e = maybe (leEvent e) eventLabel (eventByKey (leEvent e))

levelText :: LogEntry -> Text
levelText e = case [l | l <- [minBound .. maxBound], levelKey l == leLevel e] of
  l : _ -> levelLabel l
  [] -> leLevel e

-- Excel -----------------------------------------------------------------------------------

xlsx :: LogReport -> BL.ByteString
xlsx r@LogReport {..} =
  xlsxWorkbook
    DocInfo
      { diTitle = title <> " " <> lrNumber
      , diSubject = lrSite
      , diCreator = lrBy
      , diCreated = T.pack (formatTime defaultTimeLocale "%FT%TZ" (zonedTimeToUTC lrAt))
      }
    [ Sheet
        { shName = "Ringkasan"
        , shWidths = [20, 100]
        , shRows =
            [[Txt Title title], [Txt Muted lrSite], []]
              ++ [[Txt Bold k, Txt Plain v] | (k, v) <- meta r]
              ++ [[], [Txt Muted note]]
        , shTable = False
        }
    , Sheet
        { shName = "Log"
        , shWidths = [7, 19, 11, 26, 16, 14, 18, 8, 40, 45, 50, 9]
        , shRows =
            map (Txt Header) ["No", "Waktu", "Tingkat", "Peristiwa", "IP", "Pelaku", "Akun", "Metode", "Alamat", "Keterangan", "User agent", "ID log"]
              : zipWith logRow [1 ..] lrRows
        , shTable = True
        }
    ]
  where
    logRow :: Int -> LogEntry -> [Cell]
    logRow n e =
      [ Num Plain n
      , Txt Plain (leAt e)
      , Txt (levelStyle (leLevel e)) (levelText e)
      , Txt Plain (eventText e)
      , Txt Plain (leIp e)
      , Txt Plain (leActor e)
      , Txt Plain (leAccount e)
      , Txt Plain (leMethod e)
      , Txt Plain (lePath e)
      , Txt Plain (leDetail e)
      , Txt Muted (leAgent e)
      , Num Plain (leId e)
      ]
    levelStyle = \case
      "danger" -> Red
      "warn" -> Amber
      _ -> Plain

-- PDF -------------------------------------------------------------------------------------

pdf :: LogReport -> BL.ByteString
pdf r@LogReport {..} =
  pdfReport
    Report
      { rpTitle = title
      , rpSubtitle = lrSite <> " · " <> period lrAt (lfDays lrFilter)
      , rpMeta = meta r
      , rpNote = note
      , rpColumns =
          [ Column "No" 28 1
          , Column "Waktu" 72 2
          , Column "Tingkat" 50 1
          , Column "Peristiwa" 92 2
          , Column "IP" 76 2
          , Column "Akun" 86 2
          , Column "Alamat" 150 3
          , Column "Keterangan" 216 4
          ]
      , rpRows = zipWith row [1 :: Int ..] lrRows
      , rpToneColumn = 2
      , rpFooter = lrNumber <> " · SHA-256 " <> T.take 16 lrDigest <> "… · " <> lrSite
      , rpAuthor = lrBy
      , rpCreated = pdfDate lrAt
      }
  where
    row n e =
      ( tone (leLevel e)
      , [ tshow n
        , leAt e
        , levelText e
        , eventText e
        , leIp e
        , T.unwords (filter (not . T.null) [leAccount e, if T.null (leActor e) then "" else "(oleh @" <> leActor e <> ")"])
        , leMethod e <> " " <> lePath e
        , leDetail e
        ]
      )
    tone = \case
      "danger" -> Severe
      "warn" -> Alert
      _ -> Normal

-- | Tanggal PDF: "D:20261122113745+07'00'".
pdfDate :: ZonedTime -> Text
pdfDate t =
  let m = timeZoneMinutes (zonedTimeZone t)
      two n = T.justifyRight 2 '0' (tshow n)
   in "D:" <> T.pack (formatTime defaultTimeLocale "%Y%m%d%H%M%S" t)
        <> (if m < 0 then "-" else "+") <> two (abs m `div` 60) <> "'" <> two (abs m `mod` 60) <> "'"
