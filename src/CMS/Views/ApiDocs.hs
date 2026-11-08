-- | Dokumentasi API publik (/api/docs) dan spesifikasi OpenAPI 3 (/api/openapi.json),
-- keduanya dibuat dari katalog di CMS.Api sehingga selalu sesuai dengan API sebenarnya.
module CMS.Views.ApiDocs (apiDocsPage, openApiSpec) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import qualified Data.Text as T
import Lucid

import CMS.Api
import CMS.Types
import CMS.Views.Layout
import CMS.Web (siteOrigin)

apiDocsPage :: Ctx -> ApiConf -> Html ()
apiDocsPage c conf =
  siteLayout c "" "Dokumentasi API" [class_ "site"] $
    main_ [class_ "site-main"] $
      section_ [class_ "pb api-docs"] $
        div_ [class_ "wrap"] $ do
          let origin = siteOrigin c
              apis = filter (apiEnabled conf . adKey) apiCatalog
              needKey = any (not . apiPublic conf . adKey) apis
          div_ [class_ "pb-heading"] $ do
            p_ [class_ "eyebrow"] "Untuk developer"
            h1_ "Dokumentasi API"
            p_ [class_ "pb-sub"] (toHtml ("Ambil data " <> siteName c <> " (cabang, artikel, dan lainnya) dalam format JSON untuk web atau aplikasi Anda."))
          div_ [class_ "doc-grid"] $ do
            aside_ [class_ "doc-nav"] $ do
              a_ [href_ "#mulai"] "Mulai"
              a_ [href_ "#autentikasi"] "Autentikasi"
              a_ [href_ "#batas"] "Batas permintaan"
              a_ [href_ "#respons"] "Format & error"
              forM_ apis $ \a -> a_ [href_ ("#api-" <> adKey a)] (icon (adIcon a) >> toHtml (" " <> adName a))
              a_ [href_ "/api/openapi.json", class_ "doc-spec"] (icon "fa-solid fa-file-code" >> " openapi.json")
            div_ [class_ "doc-body"] $ do
              docSection "mulai" "Mulai" $ do
                p_ $ do
                  "Alamat dasar semua API: "
                  code_ (toHtml (origin <> "/api/v1"))
                  ". Semua jawaban berupa JSON (UTF-8). Spesifikasi lengkap tersedia sebagai "
                  a_ [href_ "/api/openapi.json"] "OpenAPI 3"
                  " untuk diimpor ke Postman, Insomnia, atau generator SDK."
                codeBlock ("curl " <> (if needKey then "-H \"X-API-Key: KEY_ANDA\" " else "") <> "\"" <> origin <> "/api/v1/branches?per_page=3\"")
              docSection "autentikasi" "Autentikasi" $ do
                if needKey
                  then do
                    p_ "API yang ditandai “Wajib API key” membutuhkan key dari pengelola situs. Kirim key di header:"
                    codeBlock "X-API-Key: dcms_xxxxxxxxxxxxxxxx"
                    p_ $ do
                      "Alternatif: "
                      kode "Authorization: Bearer dcms_…"
                      " atau parameter "
                      kode "?api_key=dcms_…"
                      " (kurang disarankan karena key ikut tercatat di log). Jaga kerahasiaan key; minta key baru bila bocor."
                  else p_ "Semua API saat ini publik dan tidak membutuhkan key."
              docSection "batas" "Batas permintaan" $ do
                p_ $ do
                  "Setiap key punya batas permintaan per menit (akses publik: "
                  toHtml (tshow' publicRateLimit)
                  " per menit per IP). Sisa kuota ada di header "
                  kode "X-RateLimit-Limit"
                  " dan "
                  kode "X-RateLimit-Remaining"
                  ". Bila terlampaui, API menjawab "
                  kode "429"
                  " dengan header "
                  kode "Retry-After"
                  " (detik)."
              docSection "respons" "Format & error" $ do
                p_ "Daftar dibungkus “data” dan “meta” (paginasi); detail hanya “data”:"
                codeBlock "{\n  \"data\": [ { \"id\": 1, \"name\": \"Batavia PIK\", … } ],\n  \"meta\": { \"total\": 122, \"page\": 1, \"per_page\": 50, \"total_pages\": 3 }\n}"
                p_ "Error selalu berbentuk:"
                codeBlock "{ \"error\": { \"status\": 401, \"code\": \"api_key_required\", \"message\": \"API key wajib…\" } }"
                table_ [class_ "table doc-table"] $ do
                  thead_ $ tr_ (th_ "Status" >> th_ "Arti")
                  tbody_ $ forM_ statusRows $ \(s, m) -> tr_ (td_ (kode s) >> td_ (toHtml m))
              forM_ apis $ \a ->
                section_ [class_ "doc-section card", id_ ("api-" <> adKey a)] $ do
                  div_ [class_ "doc-api-head"] $ do
                    h2_ (icon (adIcon a) >> toHtml (" " <> adName a))
                    accessBadge (apiPublic conf (adKey a))
                  p_ [class_ "muted"] (toHtml (adDesc a))
                  forM_ (adEndpoints a) $ \e -> do
                    div_ [class_ "doc-endpoint"] $ do
                      span_ [class_ "doc-method"] "GET"
                      code_ (toHtml (epPath e))
                    p_ (toHtml (epSummary e))
                    unless (null (epParams e)) $
                      table_ [class_ "table doc-table"] $ do
                        thead_ $ tr_ (th_ "Parameter" >> th_ "Tipe" >> th_ "Keterangan")
                        tbody_ $ forM_ (epParams e) $ \(n, t, d) ->
                          tr_ $ do
                            td_ (code_ (toHtml n))
                            td_ [class_ "muted"] (toHtml t)
                            td_ (toHtml d)
                  when (adKey a == "branches") $ do
                    h3_ "Contoh: cabang terdekat dari pengguna (JavaScript)"
                    codeBlock $
                      T.unlines
                        [ "navigator.geolocation.getCurrentPosition(async (pos) => {"
                        , "  const near = `${pos.coords.latitude},${pos.coords.longitude}`;"
                        , "  const res = await fetch(`" <> origin <> "/api/v1/branches?near=${near}&per_page=5`"
                            <> (if apiPublic conf "branches" then ");" else ", {")
                        ]
                        <> (if apiPublic conf "branches" then "" else "    headers: { \"X-API-Key\": \"KEY_ANDA\" },\n  });\n")
                        <> "  const { data } = await res.json();\n  data.forEach((b, i) => console.log(`${i + 1}. ${b.name} — ${b.distance_km} km`));\n});"
  where
    statusRows :: [(Text, Text)]
    statusRows =
      [ ("200", "Berhasil")
      , ("400", "Parameter tidak valid (mis. near salah format)")
      , ("401", "API key wajib, tidak valid, atau dinonaktifkan")
      , ("403", "API key tidak diizinkan memakai API ini")
      , ("404", "Data/endpoint tidak ada, atau API sedang dimatikan")
      , ("429", "Batas permintaan per menit terlampaui")
      ]
    tshow' = T.pack . show

docSection :: Text -> Text -> Html () -> Html ()
docSection sid title inner =
  section_ [class_ "doc-section card", id_ sid] $ do
    h2_ (toHtml title)
    inner

-- | Potongan kode sebaris.
kode :: Text -> Html ()
kode = code_ . toHtml

codeBlock :: Text -> Html ()
codeBlock t = pre_ [class_ "doc-code"] (code_ (toHtml t))

accessBadge :: Bool -> Html ()
accessBadge public =
  if public
    then span_ [class_ "badge badge-published"] (icon "fa-solid fa-lock-open" >> " Publik")
    else span_ [class_ "badge badge-pending"] (icon "fa-solid fa-key" >> " Wajib API key")

-- OpenAPI ------------------------------------------------------------------------------------

openApiSpec :: Text -> Text -> ApiConf -> Value
openApiSpec origin site conf =
  object
    [ "openapi" .= ("3.0.3" :: Text)
    , "info"
        .= object
          [ "title" .= (site <> " API")
          , "version" .= ("1.0.0" :: Text)
          , "description" .= ("Data " <> site <> " dalam format JSON. Dokumentasi: " <> origin <> "/api/docs")
          ]
    , "servers" .= [object ["url" .= origin]]
    , "components"
        .= object
          [ "securitySchemes"
              .= object
                [ "ApiKeyHeader" .= object ["type" .= ("apiKey" :: Text), "in" .= ("header" :: Text), "name" .= ("X-API-Key" :: Text)]
                ]
          , "schemas"
              .= object
                [ "Error"
                    .= object
                      [ "type" .= ("object" :: Text)
                      , "properties"
                          .= object
                            [ "error" .= object ["type" .= ("object" :: Text), "properties" .= object ["status" .= ty "integer", "code" .= ty "string", "message" .= ty "string"]]
                            ]
                      ]
                ]
          ]
    , "paths" .= object [Key.fromText (epPath e) .= pathItem a e | a <- apiCatalog, apiEnabled conf (adKey a), e <- adEndpoints a]
    ]
  where
    ty :: Text -> Value
    ty t = object ["type" .= t]
    pathItem a e =
      object
        [ "get"
            .= object
              [ "tags" .= [adName a]
              , "summary" .= epSummary e
              , "operationId" .= T.filter (/= '/') (T.replace "{" "by_" (T.replace "}" "" (adKey a <> T.replace "/api/v1" "" (epPath e))))
              , "parameters" .= map (param e) (epParams e)
              , "security" .= (if apiPublic conf (adKey a) then [] else [object ["ApiKeyHeader" .= ([] :: [Text])]])
              , "responses"
                  .= object
                    [ "200" .= object ["description" .= ("Berhasil" :: Text)]
                    , "401" .= errResp "API key wajib/tidak valid"
                    , "404" .= errResp "Tidak ditemukan"
                    , "429" .= errResp "Batas permintaan terlampaui"
                    ]
              ]
        ]
    errResp :: Text -> Value
    errResp d = object ["description" .= d, "content" .= object ["application/json" .= object ["schema" .= object ["$ref" .= ("#/components/schemas/Error" :: Text)]]]]
    param e (n, t, d) =
      let inPath = ("{" <> n <> "}") `T.isInfixOf` epPath e
       in object
            [ "name" .= n
            , "in" .= (if inPath then "path" else "query" :: Text)
            , "required" .= inPath
            , "description" .= d
            , "schema" .= ty t
            ]
