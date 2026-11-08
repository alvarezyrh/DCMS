-- | Menu API Gateway di dashboard (khusus Administrator): katalog API, akses, CORS,
-- API key, dan statistik pemakaian.
module CMS.Views.ApiAdmin (ApiPageData (..), apiGatewayPage) where

import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Lucid.Base (makeAttribute)

import CMS.Api
import CMS.Types
import CMS.Util
import CMS.Views.Layout
import CMS.Web (siteOrigin)

data ApiPageData = ApiPageData
  { apConf :: ApiConf
  , apKeys :: [ApiKey]
  , apUsageApi :: [(Text, Int, Int)] -- (api, total, gagal) 7 hari
  , apUsageKey :: [(Int, Int)] -- (key_id, total) 7 hari
  , apNewKey :: Maybe (Text, Text) -- (nama, key lengkap) — ditampilkan sekali
  }

apiGatewayPage :: Ctx -> User -> ApiPageData -> [Text] -> Html ()
apiGatewayPage c u ApiPageData {..} errs = adminLayout c u "api" "API Gateway" $ do
  let origin = siteOrigin c
      total = sum [n | (_, n, _) <- apUsageApi]
      failed = sum [f | (_, _, f) <- apUsageApi]
      activeApis = length (filter (apiEnabled apConf . adKey) apiCatalog)
      activeKeys = length (filter akActive apKeys)
      usageOf k = fromMaybe (0, 0) (lookup k [(a, (n, f)) | (a, n, f) <- apUsageApi])
  div_ [class_ "seo-kpis"] $ do
    kpi "fa-solid fa-arrow-right-arrow-left" "Permintaan 7 hari" (fmt total) "Semua API"
    kpi "fa-solid fa-triangle-exclamation" "Gagal" (pct failed total) (fmt failed <> " permintaan (4xx/5xx)")
    kpi "fa-solid fa-plug-circle-check" "API aktif" (tshow activeApis <> " / " <> tshow (length apiCatalog)) "Dari katalog"
    kpi "fa-solid fa-key" "API key aktif" (tshow activeKeys) (tshow (length apKeys) <> " key terdaftar")
  errorList errs
  forM_ apNewKey $ \(name, key) ->
    section_ [class_ "card new-key", role_ "alert"] $ do
      h3_ (icon "fa-solid fa-key" >> toHtml (" API key “" <> name <> "” dibuat"))
      p_ [class_ "muted"] "Salin dan simpan sekarang. Demi keamanan, key ini tidak akan ditampilkan lagi."
      div_ [class_ "copy-row"] $ do
        code_ [class_ "copy-value", id_ "new-key"] (toHtml key)
        button_ [type_ "button", class_ "btn btn-sm", data_ "copy" "#new-key"] (icon "fa-regular fa-copy" >> " Salin")
  section_ [class_ "card api-base"] $ do
    div_ $ do
      h3_ "Alamat API"
      div_ [class_ "copy-row"] $ do
        code_ [class_ "copy-value", id_ "api-base"] (toHtml (origin <> "/api/v1"))
        button_ [type_ "button", class_ "btn btn-ghost btn-sm", data_ "copy" "#api-base"] (icon "fa-regular fa-copy" >> " Salin")
    div_ [class_ "row"] $ do
      a_ [href_ "/api/docs", target_ "_blank", rel_ "noopener", class_ "btn btn-ghost"] (icon "fa-solid fa-book" >> "Dokumentasi")
      a_ [href_ "/api/openapi.json", target_ "_blank", rel_ "noopener", class_ "btn btn-ghost"] (icon "fa-solid fa-file-code" >> "OpenAPI")
  -- Katalog & pengaturan
  form_ [method_ "post", action_ "/admin/api/config", class_ "card table-wrap api-catalog"] $ do
    csrfField c
    div_ [class_ "viz-head"] $ do
      div_ $ do
        h3_ "Katalog API"
        p_ [class_ "muted"] "Aktifkan API dan tentukan siapa yang boleh memakainya."
    table_ [class_ "table"] $ do
      thead_ $ tr_ $ th_ "API" >> th_ "Endpoint" >> th_ "Aktif" >> th_ "Akses" >> th_ [class_ "right"] "7 hari"
      tbody_ $ forM_ apiCatalog $ \a -> do
        let k = adKey a
            (n, f) = usageOf k
        tr_ $ do
          td_ $ do
            strong_ (icon (adIcon a) >> toHtml (" " <> adName a))
            div_ [class_ "muted api-desc"] (toHtml (adDesc a))
          td_ $ ul_ [class_ "api-eps"] $ forM_ (adEndpoints a) $ \e -> li_ (span_ [class_ "doc-method"] "GET" >> code_ (toHtml (T.replace "/api/v1" "" (epPath e))))
          td_ $ label_ [class_ "switch-row api-switch"] $ do
            span_ [class_ "sr-only"] (toHtml ("Aktifkan API " <> adName a))
            input_ ([type_ "checkbox", name_ ("enabled_" <> k), value_ "1"] <> [checked_ | apiEnabled apConf k])
          td_ $ select_ [name_ ("access_" <> k), makeAttribute "aria-label" ("Akses API " <> adName a)] $ do
            option_ ([value_ "key"] <> [selected_ "selected" | not (apiPublic apConf k)]) "Wajib API key"
            option_ ([value_ "public"] <> [selected_ "selected" | apiPublic apConf k]) "Publik (tanpa key)"
          td_ [class_ "right mono"] $ do
            toHtml (fmt n)
            when (f > 0) $ div_ [class_ "muted"] (toHtml (fmt f <> " gagal"))
    div_ [class_ "api-cors"] $ do
      field "Domain yang boleh memanggil API dari browser (CORS)" "cors" $
        textarea_ [id_ "cors", name_ "cors", rows_ "3", placeholder_ "https://aplikasi-anda.com"] (toHtml (acCors apConf))
      hint "Satu domain per baris, mis. https://toko.contoh.id. Isi * untuk mengizinkan semua domain. Aplikasi server/mobile tidak terpengaruh CORS."
      button_ [type_ "submit", class_ "btn"] "Simpan pengaturan API"
  -- API key
  section_ [class_ "card table-wrap", id_ "keys"] $ do
    div_ [class_ "viz-head"] $
      div_ $ do
        h3_ "API key"
        p_ [class_ "muted"] "Berikan satu key untuk setiap web/aplikasi agar pemakaiannya bisa dipantau dan dicabut sendiri-sendiri."
    form_ [method_ "post", action_ "/admin/api/keys", class_ "key-form"] $ do
      csrfField c
      div_ [class_ "form-grid"] $ do
        inputField "text" "Nama key (untuk siapa?)" "name" "" [required_ "required", placeholder_ "mis. Aplikasi Android", makeAttribute "maxlength" "80"]
        inputField "number" "Batas permintaan per menit" "rate_limit" (tshow defaultRateLimit) [min_ "1", max_ "10000"]
      fieldset_ [class_ "key-scopes"] $ do
        legend_ "Boleh memakai"
        label_ (input_ [type_ "checkbox", name_ "scope", value_ "*", checked_] >> " Semua API")
        forM_ apiCatalog $ \a -> label_ (input_ [type_ "checkbox", name_ "scope", value_ (adKey a)] >> toHtml (" " <> adName a))
      button_ [type_ "submit", class_ "btn"] (icon "fa-solid fa-plus" >> " Buat API key")
    if null apKeys
      then emptyState "Belum ada API key."
      else table_ [class_ "table"] $ do
        thead_ $ tr_ $ th_ "Nama" >> th_ "Key" >> th_ "Boleh memakai" >> th_ "Batas" >> th_ [class_ "right"] "7 hari" >> th_ "Terakhir dipakai" >> th_ "Status" >> th_ [class_ "right"] "Aksi"
        tbody_ $ forM_ apKeys $ \k -> tr_ ([class_ "is-off" | not (akActive k)]) $ do
          td_ (strong_ (toHtml (akName k)) >> br_ [] >> small_ [class_ "muted"] (toHtml ("Dibuat " <> fmtDate (akCreated k))))
          td_ (code_ [class_ "mono"] (toHtml (akPrefix k <> "…")))
          td_ (toHtml (scopeLabel (akScopes k)))
          td_ [class_ "mono"] (toHtml (tshow (akRateLimit k) <> "/menit"))
          td_ [class_ "right mono"] (toHtml (fmt (fromMaybe 0 (lookup (akId k) apUsageKey))))
          td_ (maybe (span_ [class_ "muted"] "Belum pernah") (toHtml . fmtDate) (akLastUsed k))
          td_ (if akActive k then span_ [class_ "badge badge-published"] "Aktif" else span_ [class_ "badge badge-draft"] "Nonaktif")
          td_ [class_ "right actions"] $ do
            form_ [method_ "post", action_ ("/admin/api/keys/" <> tshow (akId k) <> "/toggle"), class_ "inline"] $ do
              csrfField c
              button_ [type_ "submit", class_ "btn btn-ghost btn-sm"] (if akActive k then "Nonaktifkan" else "Aktifkan")
            form_ [method_ "post", action_ ("/admin/api/keys/" <> tshow (akId k) <> "/delete"), class_ "inline", data_ "confirm" ("Hapus API key “" <> akName k <> "”? Aplikasi yang memakainya akan langsung ditolak.")] $ do
              csrfField c
              button_ [type_ "submit", class_ "btn btn-danger btn-sm"] "Hapus"
  where
    kpi :: Text -> Text -> Text -> Text -> Html ()
    kpi ic label val sub =
      div_ [class_ "card kpi"] $ do
        span_ [class_ "kpi-ico"] (icon ic)
        div_ $ do
          span_ [class_ "kpi-label"] (toHtml label)
          strong_ [class_ "kpi-value"] (toHtml val)
          small_ [class_ "muted"] (toHtml sub)
    scopeLabel s
      | "*" `elem` parts = "Semua API"
      | otherwise = T.intercalate ", " [adName a | a <- apiCatalog, adKey a `elem` parts]
      where
        parts = map T.strip (T.splitOn "," s)
    fmt :: Int -> Text
    fmt n
      | n < 1000 = tshow n
      | otherwise = T.intercalate "." (reverse (map T.reverse (T.chunksOf 3 (T.reverse (tshow n)))))
    pct :: Int -> Int -> Text
    pct _ 0 = "0%"
    pct a b = tshow (round (fromIntegral a * 100 / fromIntegral b :: Double) :: Int) <> "%"
