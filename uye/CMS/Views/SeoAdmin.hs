-- | Menu SEO di dashboard: ringkasan, analitik kunjungan, audit artikel, integrasi;
-- plus widget "SEO & Analitik" untuk halaman dashboard.
--
-- Grafik dibuat dari HTML biasa (tanpa pustaka). Warna seri: --series-1 (mesin
-- pencari) dan --series-2 (sumber lain), divalidasi untuk buta warna di mode
-- terang & gelap. Tooltip grafik ditangani static/app.js ([data-tip]).
module CMS.Views.SeoAdmin
  ( SeoData (..)
  , SeoWidget (..)
  , seoPage
  , seoWidgetCard
  , scoreRing
  , checkItem
  , issueSummary
  ) where

import Control.Monad (forM_, when)
import Data.List (find, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, defaultTimeLocale, formatTime)
import Lucid
import Lucid.Base (makeAttribute)

import CMS.Analytics (sourceLabel)
import CMS.Seo
import CMS.Types
import CMS.Util
import CMS.Views.Layout
import CMS.Web (siteOrigin)

data SeoData = SeoData
  { sdDays :: [(Day, Int, Int)] -- (tanggal, dari mesin pencari, dari sumber lain)
  , sdSources :: [(Text, Int)]
  , sdEngines :: [(Text, Int)]
  , sdTop :: [(Text, Int, Int)] -- (path, total, dari pencarian)
  , sdPosts :: [(Post, [Check], Int)] -- artikel, hasil periksa, skor
  , sdLevel :: Text -- filter audit: "" | good | warn | bad
  }

data SeoWidget = SeoWidget
  { swDays :: [(Day, Int, Int)]
  , swScores :: [Int] -- skor semua artikel
  }

-- Halaman menu SEO --------------------------------------------------------------------

seoPage :: Ctx -> User -> SeoData -> [Text] -> Html ()
seoPage c u SeoData {..} errs = adminLayout c u "seo" "SEO & Analitik" $ do
  let total = sum [s + o | (_, s, o) <- sdDays]
      search = sum [s | (_, s, _) <- sdDays]
      scores = [n | (_, _, n) <- sdPosts]
      avg = average scores
      bad = length (filter ((== Bad) . scoreLevel) scores)
  -- Ringkasan angka
  div_ [class_ "seo-kpis"] $ do
    kpi "fa-solid fa-eye" "Kunjungan 30 hari" (fmtInt total) "Pengunjung yang tidak login, tanpa bot"
    kpi "fa-brands fa-google" "Dari mesin pencari" (fmtInt search) (pctText search total <> " dari semua kunjungan")
    div_ [class_ "card kpi kpi-score"] $ do
      scoreRing "ring-md" avg
      div_ $ do
        span_ [class_ "kpi-label"] "Skor SEO rata-rata"
        levelChip (scoreLevel avg)
        small_ [class_ "muted"] (toHtml (fmtInt (length scores) <> " artikel diperiksa"))
    kpi "fa-solid fa-triangle-exclamation" "Perlu perbaikan" (fmtInt bad) "Artikel dengan skor di bawah 50"
  -- Grafik kunjungan harian
  section_ [class_ "card viz-card"] $ do
    div_ [class_ "viz-head"] $ do
      div_ $ do
        h3_ "Kunjungan harian"
        p_ [class_ "muted"] "30 hari terakhir, per sumber"
      legend
    dailyChart sdDays
    details_ [class_ "viz-table"] $ do
      summary_ "Lihat sebagai tabel"
      table_ [class_ "table"] $ do
        thead_ $ tr_ $ th_ "Tanggal" >> th_ [class_ "right"] "Mesin pencari" >> th_ [class_ "right"] "Sumber lain" >> th_ [class_ "right"] "Total"
        tbody_ $ forM_ (reverse sdDays) $ \(d, s, o) ->
          tr_ $ do
            td_ (toHtml (dayLong d))
            td_ [class_ "right"] (toHtml (fmtInt s))
            td_ [class_ "right"] (toHtml (fmtInt o))
            td_ [class_ "right"] (toHtml (fmtInt (s + o)))
  div_ [class_ "seo-cols"] $ do
    section_ [class_ "card"] $ do
      h3_ "Sumber kunjungan"
      p_ [class_ "muted"] "30 hari terakhir"
      hbars [(sourceLabel k, n) | (k, n) <- sdSources]
    section_ [class_ "card"] $ do
      h3_ "Mesin pencari"
      p_ [class_ "muted"] "Pengirim kunjungan dari pencarian"
      hbars sdEngines
  section_ [class_ "card table-wrap"] $ do
    h3_ "Halaman terpopuler"
    p_ [class_ "muted"] "30 hari terakhir"
    if null sdTop
      then emptyState "Belum ada kunjungan tercatat."
      else table_ [class_ "table"] $ do
        thead_ $ tr_ $ th_ "Halaman" >> th_ [class_ "right"] "Kunjungan" >> th_ [class_ "right"] "Dari pencarian"
        tbody_ $ forM_ sdTop $ \(p, n, s) -> tr_ $ do
          td_ $ a_ [href_ p, target_ "_blank", rel_ "noopener", class_ "mono"] (toHtml p)
          td_ [class_ "right"] (toHtml (fmtInt n))
          td_ [class_ "right"] (toHtml (fmtInt s <> " · " <> pctText s n))
  -- Audit artikel
  section_ [class_ "card table-wrap", id_ "audit"] $ do
    div_ [class_ "viz-head"] $ do
      div_ $ do
        h3_ "Audit SEO artikel"
        p_ [class_ "muted"] "Urut dari skor terendah. Klik Perbaiki untuk membuka editor."
      nav_ [class_ "seg-tabs"] $
        forM_ [("", "Semua"), ("bad", "Perlu perbaikan"), ("warn", "Cukup"), ("good", "Baik")] $ \(k, l) ->
          a_ ([href_ (if T.null k then "/admin/seo#audit" else "/admin/seo?level=" <> k <> "#audit")] <> [class_ "active" | k == sdLevel]) $ do
            toHtml (l :: Text)
            span_ [class_ "seg-n"] (toHtml (tshow (length [() | (_, _, n) <- sdPosts, T.null k || levelKey (scoreLevel n) == k])))
    let rows = sortOn (\(_, _, n) -> n) [r | r@(_, _, n) <- sdPosts, T.null sdLevel || levelKey (scoreLevel n) == sdLevel]
    if null rows
      then emptyState "Tidak ada artikel di kelompok ini."
      else table_ [class_ "table seo-audit"] $ do
        thead_ $ tr_ $ th_ "Artikel" >> th_ "Kata kunci" >> th_ "Skor" >> th_ "Yang perlu dibenahi" >> th_ [class_ "right"] ""
        tbody_ $ forM_ rows $ \(p, checks, n) -> tr_ $ do
          td_ $ do
            strong_ (toHtml (postTitle p))
            div_ [class_ "audit-meta"] $ do
              statusBadge (postStatus p)
              when (seoNoindex (postSeo p)) $ span_ [class_ "badge"] "noindex"
          td_ (if isBlank (seoKeyword (postSeo p)) then span_ [class_ "muted"] "—" else toHtml (seoKeyword (postSeo p)))
          td_ $ div_ [class_ "audit-score"] (scoreRing "ring-sm" n >> levelChip (scoreLevel n))
          td_ $ issueSummary checks
          td_ [class_ "right"] $ a_ [href_ ("/admin/posts/" <> tshow (postId p) <> "/edit#seo-panel"), class_ "btn btn-sm"] (icon "fa-solid fa-wand-magic-sparkles" >> "Perbaiki")
  when (isAdmin u) $ integrationCard c errs
  where
    kpi :: Text -> Text -> Text -> Text -> Html ()
    kpi ic label val sub =
      div_ [class_ "card kpi"] $ do
        span_ [class_ "kpi-ico"] (icon ic)
        div_ $ do
          span_ [class_ "kpi-label"] (toHtml (label :: Text))
          strong_ [class_ "kpi-value"] (toHtml (val :: Text))
          small_ [class_ "muted"] (toHtml (sub :: Text))

-- | Dua masalah terpenting (merah dulu), sisanya sebagai jumlah.
issueSummary :: [Check] -> Html ()
issueSummary checks =
  let issues = filter ((== Bad) . ckLevel) checks ++ filter ((== Warn) . ckLevel) checks
   in if null issues
        then span_ [class_ "lv-good issue-ok"] (icon "fa-solid fa-circle-check" >> " Semua beres")
        else ul_ [class_ "issue-list"] $ do
          mapM_ checkItem (take 2 issues)
          when (length issues > 2) $ li_ [class_ "muted"] (toHtml ("+" <> tshow (length issues - 2) <> " lainnya"))

integrationCard :: Ctx -> [Text] -> Html ()
integrationCard c errs = do
  let ss = setSeo (cSettings c)
      s = cSettings c
      origin = siteOrigin c
  section_ [class_ "card", id_ "integrasi"] $ do
    h3_ (icon "fa-solid fa-plug" >> " Integrasi & pengaturan SEO")
    p_ [class_ "muted"] "Hubungkan situs ke Google & Bing, lalu kirim sitemap agar artikel cepat terindeks."
    errorList errs
    div_ [class_ "seo-cols"] $ do
      form_ [method_ "post", action_ "/admin/seo/settings", class_ "stack"] $ do
        csrfField c
        inputField "text" "Kode verifikasi Google Search Console" "seo_google" (ssGoogleVerify ss) [placeholder_ "Tempel kode atau tag <meta> dari Google"]
        hint "Search Console → Tambahkan properti → Awalan URL → metode Tag HTML."
        inputField "text" "Kode verifikasi Bing Webmaster" "seo_bing" (ssBingVerify ss) [placeholder_ "Tempel kode atau tag <meta> dari Bing"]
        inputField "text" "ID Google Analytics 4" "seo_ga" (ssGaId ss) [placeholder_ "G-XXXXXXXXXX"]
        hint "Opsional. Kosongkan bila cukup memakai analitik bawaan di halaman ini."
        inputField "text" "Gambar pratinjau bawaan (URL)" "seo_image" (ssDefaultImage ss) [placeholder_ "https://… atau /asset/img/…"]
        hint "Dipakai saat halaman/artikel tanpa gambar dibagikan di WhatsApp & media sosial."
        checkboxField "Izinkan mesin pencari mengindeks situs" "seo_index" (ssIndexable ss)
        hint "Matikan hanya saat situs masih tahap pengerjaan."
        button_ [type_ "submit", class_ "btn"] "Simpan pengaturan SEO"
      div_ [class_ "seo-status"] $ do
        h4_ "Berkas untuk mesin pencari"
        fileRow "fa-solid fa-sitemap" "Sitemap" (origin <> "/sitemap.xml") "Kirim alamat ini di Search Console → Sitemaps."
        fileRow "fa-solid fa-robot" "robots.txt" (origin <> "/robots.txt") "Dibuat otomatis; dashboard & login tidak diindeks."
        h4_ "Kesiapan situs"
        ul_ [class_ "seo-checks"] $ do
          readiness (ssIndexable ss) "Situs boleh diindeks" "Pengindeksan dimatikan"
          readiness (not (isBlank (setDescription s))) "Deskripsi situs sudah diisi" "Isi deskripsi situs di menu Pengaturan"
          readiness (not (T.null (ssGoogleVerify ss))) "Terhubung ke Google Search Console" "Belum terhubung ke Google Search Console"
          readiness (not (T.null (ssDefaultImage ss))) "Ada gambar pratinjau bawaan" "Belum ada gambar pratinjau bawaan"
          readiness (not (null (setSocial s))) "Media sosial terhubung di footer" "Tambahkan media sosial di menu Pengaturan"
  where
    fileRow :: Text -> Text -> Text -> Text -> Html ()
    fileRow ic label url help =
      div_ [class_ "file-row"] $ do
        span_ [class_ "kpi-ico"] (icon ic)
        div_ $ do
          strong_ (toHtml (label :: Text))
          a_ [href_ url, target_ "_blank", rel_ "noopener", class_ "mono"] (toHtml url)
          small_ [class_ "muted"] (toHtml (help :: Text))
    readiness :: Bool -> Text -> Text -> Html ()
    readiness ok good bad = checkItem (if ok then Check Good good else Check Warn bad)

-- Widget dashboard ----------------------------------------------------------------------

seoWidgetCard :: SeoWidget -> Html ()
seoWidgetCard SeoWidget {..} =
  section_ [class_ "card seo-widget"] $ do
    div_ [class_ "cal-head"] $ do
      h3_ (icon "fa-solid fa-chart-line" >> " SEO & Analitik")
      a_ [href_ "/admin/seo", class_ "btn btn-ghost btn-sm"] "Buka menu SEO"
    let last7 = drop (length swDays - 7) swDays
        total7 = sum [s + o | (_, s, o) <- last7]
        search7 = sum [s | (_, s, _) <- last7]
        avg = average swScores
        count lv = length (filter ((== lv) . scoreLevel) swScores)
    div_ [class_ "sw-top"] $ do
      div_ [class_ "sw-hero"] $ do
        span_ [class_ "kpi-label"] "Kunjungan 7 hari"
        strong_ [class_ "kpi-value"] (toHtml (fmtInt total7))
        small_ [class_ "muted"] $ do
          span_ [class_ "dot s1"] ""
          toHtml (" " <> fmtInt search7 <> " dari mesin pencari (" <> pctText search7 total7 <> ")")
      div_ [class_ "sw-score"] $ do
        scoreRing "ring-md" avg
        div_ $ do
          span_ [class_ "kpi-label"] "Skor SEO rata-rata"
          levelChip (scoreLevel avg)
    dailyChart swDays
    legend
    div_ [class_ "sw-levels"] $
      forM_ [Good, Warn, Bad] $ \lv ->
        a_ [href_ ("/admin/seo?level=" <> levelKey lv <> "#audit"), class_ ("sw-level " <> levelClass lv)] $ do
          i_ [class_ (levelIcon lv), makeAttribute "aria-hidden" "true"] ""
          strong_ (toHtml (tshow (count lv)))
          span_ (toHtml (levelLabel lv))

-- Komponen ------------------------------------------------------------------------------

legend :: Html ()
legend =
  div_ [class_ "viz-legend"] $ do
    span_ (span_ [class_ "dot s1"] "" >> " Mesin pencari")
    span_ (span_ [class_ "dot s2"] "" >> " Sumber lain")

-- | Batang bertumpuk per hari: mesin pencari (bawah) + sumber lain. Satu sumbu.
dailyChart :: [(Day, Int, Int)] -> Html ()
dailyChart days =
  div_ [class_ "vchart", role_ "img", makeAttribute "aria-label" ("Grafik kunjungan " <> tshow (length days) <> " hari, total " <> tshow total)] $ do
    div_ [class_ "vchart-y", makeAttribute "aria-hidden" "true"] $
      forM_ [top, top `div` 2, 0] $ \v -> span_ (toHtml (fmtInt v))
    div_ [class_ "vchart-plot"] $ do
      div_ [class_ "vchart-grid", makeAttribute "aria-hidden" "true"] (span_ "" >> span_ "" >> span_ "")
      div_ [class_ "vchart-cols"] $
        forM_ (zip [0 :: Int ..] days) $ \(i, (d, s, o)) ->
          div_
            [ class_ "vcol"
            , tabindex_ "0"
            , data_ "tip" (dayLong d <> "\nMesin pencari: " <> fmtInt s <> "\nSumber lain: " <> fmtInt o <> "\nTotal: " <> fmtInt (s + o))
            ]
            $ do
              -- Anak pertama = segmen teratas (ujungnya dibulatkan lewat CSS).
              when (o > 0) $ div_ [class_ "vseg s2", style_ (pctH o)] ""
              when (s > 0) $ div_ [class_ "vseg s1", style_ (pctH s)] ""
              when (labelAt i) $ span_ [class_ "vx", makeAttribute "aria-hidden" "true"] (toHtml (dayShort d))
  where
    total = sum [s + o | (_, s, o) <- days]
    top = niceMax (maximum (1 : [s + o | (_, s, o) <- days]))
    pctH n = "height:" <> tshow (fromIntegral n * 100 / fromIntegral top :: Double) <> "%"
    n = length days
    step = if n > 14 then 7 else if n > 7 then 3 else 1
    labelAt i = (n - 1 - i) `mod` step == 0

-- | Batang horizontal satu warna (besaran per kategori), angka di ujung.
hbars :: [(Text, Int)] -> Html ()
hbars rows
  | all ((== 0) . snd) rows = emptyState "Belum ada data."
  | otherwise =
      ul_ [class_ "hbars"] $ forM_ rows $ \(label, v) ->
        li_ $ do
          span_ [class_ "hb-label"] (toHtml (if T.null label then "Lainnya" else label))
          span_ [class_ "hb-track"] $ span_ [class_ "hb-bar", style_ ("width:" <> tshow (fromIntegral v * 100 / fromIntegral mx :: Double) <> "%")] ""
          span_ [class_ "hb-val"] (toHtml (fmtInt v <> " · " <> pctText v tot))
  where
    mx = maximum (1 : map snd rows)
    tot = sum (map snd rows)

-- | Cincin skor 0–100 (SVG). Warna status + label di sebelahnya (tidak hanya warna).
scoreRing :: Text -> Int -> Html ()
scoreRing cls n =
  div_ [class_ ("score-ring " <> cls <> " " <> levelClass (scoreLevel n)), data_ "score" (tshow n), role_ "img", makeAttribute "aria-label" ("Skor SEO " <> tshow n <> " dari 100")] $ do
    toHtmlRaw
      ( "<svg viewBox=\"0 0 36 36\" aria-hidden=\"true\"><circle class=\"ring-bg\" cx=\"18\" cy=\"18\" r=\"15.9\"/>\
        \<circle class=\"ring-fg\" cx=\"18\" cy=\"18\" r=\"15.9\" pathLength=\"100\" stroke-dasharray=\""
          <> tshow n
          <> " 100\"/></svg>" ::
          Text
      )
    span_ [class_ "score-num"] (toHtml (tshow n))

checkItem :: Check -> Html ()
checkItem ck =
  li_ [class_ (levelClass (ckLevel ck))] $ do
    i_ [class_ (levelIcon (ckLevel ck)), makeAttribute "aria-hidden" "true"] ""
    span_ (toHtml (ckText ck))

levelChip :: Level -> Html ()
levelChip lv = span_ [class_ ("seo-level " <> levelClass lv)] (toHtml (levelLabel lv))

levelIcon :: Level -> Text
levelIcon = \case
  Good -> "fa-solid fa-circle-check"
  Warn -> "fa-solid fa-circle-exclamation"
  Bad -> "fa-solid fa-circle-xmark"

levelKey :: Level -> Text
levelKey = \case
  Good -> "good"
  Warn -> "warn"
  Bad -> "bad"

-- Format ---------------------------------------------------------------------------------

average :: [Int] -> Int
average [] = 0
average xs = round (fromIntegral (sum xs) / fromIntegral (length xs) :: Double)

-- | Batas atas sumbu yang "bulat": 1, 2, 5 × 10^n.
niceMax :: Int -> Int
niceMax v = fromMaybe v (find (>= v) [b * 10 ^ e | e <- [0 :: Int .. 12], b <- [1, 2, 5]])

fmtInt :: Int -> Text
fmtInt n
  | n < 0 = "-" <> fmtInt (negate n)
  | otherwise = T.intercalate "." (reverse (map T.reverse (T.chunksOf 3 (T.reverse (tshow n)))))

pctText :: Int -> Int -> Text
pctText _ 0 = "0%"
pctText a b = tshow (round (fromIntegral a * 100 / fromIntegral b :: Double) :: Int) <> "%"

dayShort :: Day -> Text
dayShort d = T.pack (formatTime defaultTimeLocale "%-d" d) <> " " <> monthShort d

dayLong :: Day -> Text
dayLong d = T.pack (formatTime defaultTimeLocale "%-d" d) <> " " <> monthShort d <> " " <> T.pack (formatTime defaultTimeLocale "%Y" d)

monthShort :: Day -> Text
monthShort d =
  let m = fromMaybe 1 (readInt (T.pack (formatTime defaultTimeLocale "%m" d)))
   in fromMaybe "" (lookup m (zip [1 ..] ["Jan", "Feb", "Mar", "Apr", "Mei", "Jun", "Jul", "Agu", "Sep", "Okt", "Nov", "Des"]))
