-- | Menu Log di dashboard (khusus Administrator): ringkasan 24 jam, IP paling
-- mencurigakan, dan daftar log keamanan yang bisa difilter.
module CMS.Views.LogAdmin (LogPageData (..), logPage) where

import Control.Monad (forM_, unless, when)
import Data.Text (Text)
import qualified Data.Text as T
import Lucid

import CMS.Log
import CMS.LogReport (exportLimit)
import CMS.Types
import CMS.Util
import CMS.Views.Layout
import CMS.Web (urlWith)

data LogPageData = LogPageData
  { lpFilter :: LogFilter
  , lpRows :: [LogEntry]
  , lpTotal :: Int
  , lpPage :: Int
  , lpPages :: Int
  , lpCounts :: [(Text, Int)] -- (peristiwa, jumlah) 24 jam terakhir
  , lpIps :: [(Text, Int, Int, Text, Text)] -- lihat DB.suspiciousIps
  }

logPage :: Ctx -> User -> LogPageData -> Html ()
logPage c u LogPageData {lpFilter = LogFilter {..}, ..} = adminLayout c u "log" "Log keamanan" $ do
  div_ [class_ "seo-kpis"] $ do
    kpiCard "fa-solid fa-user-xmark" "Login gagal" (count24 [LoginFail, LoginInactive]) "24 jam terakhir"
    kpiCard "fa-solid fa-lock" "Login diblokir" (count24 [LoginBlocked]) "Terlalu banyak password salah"
    kpiCard "fa-solid fa-bug" "Pemindaian celah" (count24 [Probe]) "Permintaan ke alamat rawan"
    kpiCard "fa-solid fa-ban" "Akses ditolak" (count24 [Forbidden, CsrfFail, ApiDenied, ApiLimited]) "Dashboard, formulir, dan API"
  section_ [class_ "card table-wrap log-card"] $ do
    div_ [class_ "viz-head"] $
      div_ $ do
        h3_ "IP paling mencurigakan"
        p_ [class_ "muted"] "Peringatan dan bahaya dalam 7 hari terakhir. Klik IP untuk melihat semua aktivitasnya."
    if null lpIps
      then emptyState "Tidak ada aktivitas mencurigakan dalam 7 hari terakhir."
      else table_ [class_ "table"] $ do
        thead_ $ tr_ $ th_ "IP" >> th_ [class_ "right"] "Peristiwa" >> th_ [class_ "right"] "Login gagal" >> th_ "Jenis" >> th_ "Terakhir"
        tbody_ $ forM_ lpIps $ \(ip, total, fails, evs, lastAt) -> tr_ $ do
          td_ (a_ [href_ (urlWith "/admin/log" [("q", ip), ("days", "30")]), class_ "mono strong"] (toHtml ip))
          td_ [class_ "right mono"] (toHtml (fmtInt total))
          td_ [class_ "right mono"] (toHtml (fmtInt fails))
          td_ (toHtml (T.intercalate ", " (map label (T.splitOn "," evs))))
          td_ [class_ "nowrap"] (toHtml (fmtDateTime lastAt))
  div_ [class_ "toolbar"] $ do
    form_ [method_ "get", action_ "/admin/log", class_ "filters"] $ do
      input_ [type_ "search", name_ "q", value_ lfSearch, placeholder_ "Cari IP, akun, alamat…"]
      choice "level" lfLevel [("", "Semua tingkat"), ("alert", "Peringatan & bahaya"), ("danger", "Bahaya"), ("warn", "Peringatan"), ("info", "Info")]
      choice "event" lfEvent (("", "Semua peristiwa") : [(eventKey e, eventLabel e) | e <- [minBound .. maxBound]])
      choice "days" (tshow lfDays) [("1", "24 jam"), ("7", "7 hari"), ("30", "30 hari"), ("0", "Semua (180 hari)")]
      button_ [type_ "submit", class_ "btn btn-ghost"] "Filter"
      when filtered $ a_ [href_ "/admin/log", class_ "btn btn-ghost"] "Reset"
    div_ [class_ "row"] $ do
      download "xlsx" "fa-regular fa-file-excel" "Excel"
      download "pdf" "fa-regular fa-file-pdf" "PDF"
  p_ [class_ "muted"] (toHtml (fmtInt lpTotal <> " catatan"))
  if null lpRows
    then emptyState "Tidak ada catatan yang cocok."
    else div_ [class_ "card table-wrap"] $ table_ [class_ "table log-table"] $ do
      thead_ $ tr_ $ th_ "Waktu" >> th_ "Tingkat" >> th_ "Peristiwa" >> th_ "IP" >> th_ "Akun" >> th_ "Alamat" >> th_ "Keterangan"
      tbody_ $ forM_ lpRows $ \e -> tr_ $ do
        td_ [class_ "nowrap"] (toHtml (fmtDateTime (leAt e)))
        td_ (levelBadge (leLevel e))
        td_ [class_ "strong"] (toHtml (label (leEvent e)))
        td_ (a_ [href_ (urlWith "/admin/log" [("q", leIp e), ("days", "30")]), class_ "mono"] (toHtml (leIp e)))
        td_ $ do
          toHtml (orDash (leAccount e))
          unless (T.null (leActor e)) $ small_ [class_ "muted log-sub"] (toHtml ("oleh @" <> leActor e))
        td_ (span_ [class_ "mono log-clip", title_ (lePath e)] (toHtml (leMethod e <> " " <> lePath e)))
        td_ $ do
          toHtml (orDash (leDetail e))
          unless (T.null (leAgent e)) $ small_ [class_ "muted log-sub log-clip", title_ (leAgent e)] (toHtml (leAgent e))
  pagination "/admin/log" filterParams lpPage lpPages
  where
    filterParams = [("q", lfSearch), ("level", lfLevel), ("event", lfEvent), ("days", tshow lfDays)]
    download :: Text -> Text -> Text -> Html ()
    download fmt ic name =
      a_
        [ href_ (urlWith "/admin/log/export" (("format", fmt) : filterParams))
        , class_ "btn btn-ghost"
        , title_ ("Unduh laporan " <> name <> " sesuai filter (maks. " <> fmtInt exportLimit <> " catatan terbaru)")
        ]
        (icon ic >> toHtml (" " <> name))
    count24 evs = fmtInt (sum [n | (k, n) <- lpCounts, k `elem` map eventKey evs])
    label k = maybe k eventLabel (eventByKey k)
    filtered = not (T.null lfSearch && T.null lfLevel && T.null lfEvent) || lfDays /= 7
    orDash t = if T.null t then "—" else t
    choice :: Text -> Text -> [(Text, Text)] -> Html ()
    choice name cur opts =
      select_ [name_ name] $
        forM_ opts $ \(v, l) -> option_ ([value_ v] <> [selected_ "selected" | v == cur]) (toHtml l)

levelBadge :: Text -> Html ()
levelBadge k = case [l | l <- [minBound .. maxBound], levelKey l == k] of
  l : _ -> span_ [class_ ("badge lv-" <> k)] (toHtml (levelLabel l))
  [] -> toHtml k
