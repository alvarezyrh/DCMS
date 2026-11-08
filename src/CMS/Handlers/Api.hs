-- | API Gateway: REST API JSON di /api/v1/… untuk web/aplikasi lain.
--
-- Setiap permintaan melewati: API aktif? → API key (wajib/tidak, valid, cakupan) →
-- batas permintaan per menit → jawaban JSON + pencatatan pemakaian. CORS diatur di
-- menu API Gateway. Dokumentasi: /api/docs, spesifikasi: /api/openapi.json.
module CMS.Handlers.Api (apiRoute, apiPreflight, newApiKey) where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar (modifyMVar)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Types
import Network.Wai (Response, requestHeaders, responseLBS)

import CMS.Api
import CMS.Auth (hashToken, randomToken)
import CMS.DB
import CMS.Seo (effectiveDesc, firstImage)
import CMS.Types
import CMS.Util
import CMS.Views.ApiDocs (apiDocsPage, openApiSpec)
import CMS.Views.Public (isoTime)
import CMS.Web

-- | /api/…
apiRoute :: Ctx -> [Text] -> IO Response
apiRoute c path = do
  conf <- decodeApiConf <$> withDb (cEnv c) getApiConfig
  case path of
    ["docs"] -> ok (apiDocsPage c conf)
    ["openapi.json"] -> pure (jsonResp status200 (corsHeaders c conf) (openApiSpec (siteOrigin c) (setSiteName (cSettings c)) conf))
    ("v1" : api : rest) -> gateway c conf api rest
    _ -> pure (apiError c conf status404 "not_found" "Endpoint tidak ditemukan. Lihat /api/docs.")

-- | Preflight CORS (OPTIONS /api/…).
apiPreflight :: Ctx -> IO Response
apiPreflight c = do
  conf <- decodeApiConf <$> withDb (cEnv c) getApiConfig
  pure $
    responseLBS
      status204
      ( corsHeaders c conf
          ++ [ ("Access-Control-Allow-Methods", "GET, OPTIONS")
             , ("Access-Control-Allow-Headers", "X-API-Key, Authorization, Content-Type")
             , ("Access-Control-Max-Age", "86400")
             ]
      )
      ""

-- | Key baru: (key lengkap untuk ditampilkan sekali, awalan, hash untuk disimpan).
newApiKey :: IO (Text, Text, Text)
newApiKey = do
  tok <- randomToken
  let full = "dcms_" <> T.take 40 tok
  pure (full, T.take 11 full, hashToken full)

-- Gateway -----------------------------------------------------------------------------------

gateway :: Ctx -> ApiConf -> Text -> [Text] -> IO Response
gateway c conf api rest = case lookupApi api of
  Nothing -> pure (apiError c conf status404 "not_found" ("API \"" <> T.take 40 api <> "\" tidak ada. Lihat /api/docs."))
  Just _
    | not (apiEnabled conf api) -> pure (apiError c conf status404 "api_disabled" "API ini sedang tidak aktif.")
    | otherwise -> do
        auth <- authenticate c conf api
        case auth of
          Left (st, code, msg) -> do
            withDb (cEnv c) $ \db -> recordApiHit db 0 api (statusCode st)
            pure (apiError c conf st code msg)
          Right mKey -> do
            (allowed, limit, remaining, retry) <- rateLimit c mKey
            let keyId = maybe 0 akId mKey
                rlHeaders = [("X-RateLimit-Limit", tshowB limit), ("X-RateLimit-Remaining", tshowB remaining)]
            if not allowed
              then do
                withDb (cEnv c) $ \db -> recordApiHit db keyId api 429
                pure $
                  addHeaders (("Retry-After", tshowB retry) : rlHeaders) $
                    apiError c conf status429 "rate_limited" ("Terlalu banyak permintaan. Batas " <> tshow limit <> " per menit; coba lagi dalam " <> tshow retry <> " detik.")
              else do
                (st, body) <- endpoint c api rest
                withDb (cEnv c) $ \db -> do
                  recordApiHit db keyId api (statusCode st)
                  mapM_ (touchApiKey db . akId) mKey
                let cache = if isJust mKey then "private, max-age=60" else "public, max-age=60"
                pure (jsonResp st (corsHeaders c conf ++ rlHeaders ++ [("Cache-Control", cache)]) body)

-- | Key dari header X-API-Key, "Authorization: Bearer …", atau ?api_key=…
authenticate :: Ctx -> ApiConf -> Text -> IO (Either (Status, Text, Text) (Maybe ApiKey))
authenticate c conf api = case presented of
  Nothing
    | apiPublic conf api -> pure (Right Nothing)
    | otherwise -> pure (Left (status401, "api_key_required", "API key wajib. Kirim di header X-API-Key."))
  Just k -> do
    mk <- withDb (cEnv c) (`findApiKeyByHash` hashToken k)
    pure $ case mk of
      Nothing -> Left (status401, "invalid_api_key", "API key tidak valid.")
      Just key
        | not (akActive key) -> Left (status401, "api_key_disabled", "API key ini dinonaktifkan.")
        | not (allows key) -> Left (status403, "forbidden", "API key ini tidak diizinkan memakai API \"" <> api <> "\".")
        | otherwise -> Right (Just key)
  where
    hdr n = T.strip . TE.decodeUtf8Lenient <$> lookup n (requestHeaders (cReq c))
    bearer = hdr "Authorization" >>= T.stripPrefix "Bearer " . T.strip
    presented = mfilter' (not . T.null) (hdr "X-API-Key" <|> bearer <|> lookup "api_key" (cQuery c))
    mfilter' p m = case m of
      Just x | p x -> Just x
      _ -> Nothing
    allows key = let s = map T.strip (T.splitOn "," (akScopes key)) in "*" `elem` s || api `elem` s

-- | Jendela tetap per menit: per API key, atau per IP untuk akses publik.
-- Hasil: (boleh?, batas, sisa, detik sampai menit berikutnya).
rateLimit :: Ctx -> Maybe ApiKey -> IO (Bool, Int, Int, Int)
rateLimit c mKey = do
  now <- getPOSIXTime
  let secs = floor now :: Integer
      minute = secs `div` 60
      retry = fromIntegral (60 - secs `mod` 60)
      bucket = maybe ("ip:" <> clientIp c) (("key:" <>) . tshow . akId) mKey
      limit = maybe publicRateLimit (max 1 . akRateLimit) mKey
  n <- modifyMVar (envApiCounter (cEnv c)) $ \m -> do
    let cur = case M.lookup bucket m of
          Just (mi, k) | mi == minute -> k + 1
          _ -> 1
        m' = M.insert bucket (minute, cur) (if M.size m > 20000 then M.filter ((== minute) . fst) m else m)
    pure (m', cur)
  pure (n <= limit, limit, max 0 (limit - n), retry)

-- Endpoint ----------------------------------------------------------------------------------

endpoint :: Ctx -> Text -> [Text] -> IO (Status, Value)
endpoint c api rest = case (api, rest) of
  ("branches", []) -> branchList c
  ("branches", ["provinces"]) -> do
    bs <- withDb (cEnv c) listActiveBranches
    -- Urutan provinsi mengikuti cabang pertamanya (sama seperti halaman /branch).
    let provs = foldl (\acc b -> if brProvince b `elem` acc then acc else acc ++ [brProvince b]) [] bs
    pure (status200, object ["data" .= [object ["province" .= p, "count" .= length (filter ((== p) . brProvince) bs)] | p <- provs], "meta" .= object ["total" .= length provs]])
  ("branches", [i]) | Just bid <- readInt i -> do
    mb <- withDb (cEnv c) (`findBranch` bid)
    pure $ case mb of
      Just b | brActive b -> (status200, object ["data" .= branchJson c Nothing b])
      _ -> notFoundJ "Cabang tidak ditemukan."
  ("posts", []) -> postList c
  ("posts", [slug]) -> do
    mp <- withDb (cEnv c) (`findPublishedPost` slug)
    pure $ case mp of
      Just p -> (status200, object ["data" .= postJson c True p])
      Nothing -> notFoundJ "Artikel tidak ditemukan."
  ("categories", []) -> do
    cats <- withDb (cEnv c) listCategories
    pure (status200, object ["data" .= map (catJson c) cats, "meta" .= object ["total" .= length cats]])
  ("site", []) -> pure (status200, object ["data" .= siteJson c])
  _ -> pure (notFoundJ "Endpoint tidak ditemukan. Lihat /api/docs.")
  where
    notFoundJ msg = (status404, errorBody 404 "not_found" msg)

branchList :: Ctx -> IO (Status, Value)
branchList c = do
  bs <- withDb (cEnv c) listActiveBranches
  let q = T.toLower (paramT c "q")
      prov = T.toLower (paramT c "province")
      city = T.toLower (paramT c "city")
      hay b = T.toLower (T.unwords [brName b, brCity b, brProvince b, brAddress b, brPhone b])
      filtered = [b | b <- bs, T.null q || all (`T.isInfixOf` hay b) (T.words q), T.null prov || T.toLower (brProvince b) == prov, T.null city || T.toLower (brCity b) == city]
      near = parseCoords (paramT c "near")
      radius = paramDouble "radius_km"
      dist b = haversineKm <$> near <*> ((,) <$> brLat b <*> brLng b)
      ranked = case near of
        Nothing -> map (\b -> (b, Nothing)) filtered
        Just _ ->
          let withD = map (\b -> (b, dist b)) filtered
              inR = maybe withD (\r -> [x | x@(_, Just d) <- withD, d <= r]) radius
           in sortOn (\(_, d) -> fromMaybe (1 / 0) d) inR
  if not (T.null (paramT c "near")) && not (isJust near)
    then pure (status400, errorBody 400 "bad_request" "Parameter near harus berbentuk \"lat,lng\", mis. -6.2088,106.8456.")
    else do
      let (page, per, items, meta) = paginate 50 200 ranked
      pure
        ( status200
        , object
            [ "data" .= [branchJson c d b | (b, d) <- items]
            , "meta" .= object (meta ++ ["page" .= page, "per_page" .= per] ++ ["near" .= object ["lat" .= la, "lng" .= lo] | Just (la, lo) <- [near]])
            ]
        )
  where
    paramDouble k = case reads (T.unpack (paramT c k)) :: [(Double, String)] of
      [(v, "")] | v > 0 -> Just v
      _ -> Nothing
    paginate :: Int -> Int -> [a] -> (Int, Int, [a], [(Key.Key, Value)])
    paginate def mx xs =
      let per = maybe def (max 1 . min mx) (paramInt c "per_page")
          page = maybe 1 (max 1) (paramInt c "page")
          total = length xs
       in (page, per, take per (drop ((page - 1) * per) xs), ["total" .= total, "total_pages" .= max 1 ((total + per - 1) `div` per)])

postList :: Ctx -> IO (Status, Value)
postList c = do
  let per = maybe 10 (max 1 . min 50) (paramInt c "per_page")
      page = maybe 1 (max 1) (paramInt c "page")
      catSlug = paramT c "category"
  mcat <- if T.null catSlug then pure Nothing else withDb (cEnv c) (`findCategoryBySlug` catSlug)
  if not (T.null catSlug) && not (isJust mcat)
    then pure (status200, object ["data" .= ([] :: [Value]), "meta" .= object ["total" .= (0 :: Int), "page" .= page, "per_page" .= per, "total_pages" .= (1 :: Int)]])
    else do
      (ps, total) <- withDb (cEnv c) $ \db ->
        listPosts db noFilter {fStatus = Just Published, fCategory = catId <$> mcat, fSearch = nonEmpty (paramT c "q")} ByPublished page per
      pure (status200, object ["data" .= map (postJson c False) ps, "meta" .= object ["total" .= total, "page" .= page, "per_page" .= per, "total_pages" .= max 1 ((total + per - 1) `div` per)]])

-- JSON ---------------------------------------------------------------------------------------

branchJson :: Ctx -> Maybe Double -> Branch -> Value
branchJson c mDist Branch {..} =
  object $
    [ "id" .= brId
    , "order" .= brSort
    , "name" .= brName
    , "city" .= brCity
    , "province" .= brProvince
    , "address" .= brAddress
    , "phone" .= nullIfBlank (fmtPhone brPhone)
    , "phone2" .= nullIfBlank (fmtPhone brPhone2)
    , "whatsapp" .= nullIfBlank wa
    , "whatsapp_url" .= (if T.null wa then Null else String ("https://wa.me/" <> wa))
    , "hours" .= nullIfBlank brHours
    , "note" .= nullIfBlank brNote
    , "maps_url" .= nullIfBlank brMapsUrl
    , "location" .= case (brLat, brLng) of
        (Just la, Just lo) -> object ["lat" .= la, "lng" .= lo]
        _ -> Null
    , "page_url" .= (siteOrigin c <> "/branch?cari=" <> T.replace " " "+" brName)
    , "updated_at" .= isoTime brUpdated
    ]
      ++ ["distance_km" .= (fromIntegral (round (d * 10) :: Int) / 10 :: Double) | Just d <- [mDist]]
  where
    wa = if isBlank brWhatsapp then waNumber brPhone else T.filter (`elem` ['0' .. '9']) brWhatsapp

postJson :: Ctx -> Bool -> Post -> Value
postJson c full p =
  object $
    [ "id" .= postId p
    , "title" .= postTitle p
    , "slug" .= postSlug p
    , "url" .= (origin <> "/post/" <> postSlug p)
    , "excerpt" .= (if isBlank (postExcerpt p) then plainExcerpt 200 (postBody p) else postExcerpt p)
    , "category" .= case (postCategoryName p, postCategorySlug p) of
        (Just n, Just s) -> object ["name" .= n, "slug" .= s]
        _ -> Null
    , "author" .= postAuthorName p
    , "published_at" .= fmap isoTime (postPublished p)
    , "updated_at" .= isoTime (postUpdated p)
    , "image" .= fmap absolute image
    , "seo" .= object ["title" .= nullIfBlank (seoTitle s), "description" .= effectiveDesc (postExcerpt p) (postBody p) s, "keyword" .= nullIfBlank (seoKeyword s)]
    ]
      ++ (if full then ["body_markdown" .= postBody p, "body_html" .= renderMarkdown (postBody p)] else [])
  where
    s = postSeo p
    origin = siteOrigin c
    image = if isBlank (seoImage s) then firstImage (postBody p) else Just (seoImage s)
    absolute u = if "/" `T.isPrefixOf` u && not ("//" `T.isPrefixOf` u) then origin <> u else u

catJson :: Ctx -> Category -> Value
catJson c k =
  object ["id" .= catId k, "name" .= catName k, "slug" .= catSlug k, "description" .= nullIfBlank (catDesc k), "post_count" .= catCount k, "url" .= (siteOrigin c <> "/category/" <> catSlug k)]

siteJson :: Ctx -> Value
siteJson c =
  object
    [ "name" .= setSiteName s
    , "tagline" .= nullIfBlank (setTagline s)
    , "description" .= nullIfBlank (setDescription s)
    , "url" .= origin
    , "logo" .= (if T.null (setLogo s) then Null else String (origin <> setLogo s))
    , "whatsapp" .= nullIfBlank (setWhatsapp s)
    , "whatsapp_url" .= (if T.null (setWhatsapp s) then Null else String ("https://wa.me/" <> setWhatsapp s))
    , "social" .= [object ["type" .= k, "url" .= u] | (k, u) <- setSocial s]
    ]
  where
    s = cSettings c
    origin = siteOrigin c

nullIfBlank :: Text -> Value
nullIfBlank t = if isBlank t then Null else String (T.strip t)

-- Respons ------------------------------------------------------------------------------------

errorBody :: Int -> Text -> Text -> Value
errorBody st code msg = object ["error" .= object ["status" .= st, "code" .= code, "message" .= msg]]

apiError :: Ctx -> ApiConf -> Status -> Text -> Text -> Response
apiError c conf st code msg = jsonResp st (corsHeaders c conf) (errorBody (statusCode st) code msg)

jsonResp :: Status -> ResponseHeaders -> Value -> Response
jsonResp st hs v = responseLBS st ((hContentType, "application/json; charset=utf-8") : hs) (encode v)

addHeaders :: ResponseHeaders -> Response -> Response
addHeaders hs r = foldr addHeader r hs

-- | CORS: "*" atau hanya origin yang terdaftar di menu API Gateway.
corsHeaders :: Ctx -> ApiConf -> ResponseHeaders
corsHeaders c conf =
  ("Access-Control-Expose-Headers", "X-RateLimit-Limit, X-RateLimit-Remaining, Retry-After") : case corsOrigins conf of
    Nothing -> [("Access-Control-Allow-Origin", "*")]
    Just allowed -> case TE.decodeUtf8Lenient <$> lookup "Origin" (requestHeaders (cReq c)) of
      Just o | T.dropWhileEnd (== '/') o `elem` allowed -> [("Access-Control-Allow-Origin", TE.encodeUtf8 o), ("Vary", "Origin")]
      _ -> [("Vary", "Origin")]

tshowB :: Int -> B.ByteString
tshowB = TE.encodeUtf8 . tshow
