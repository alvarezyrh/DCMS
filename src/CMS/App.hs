-- | Aplikasi WAI: parsing request, sesi, CSRF, routing, dan seed awal.
module CMS.App (mkApp, seedIfEmpty) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import qualified Data.ByteString as B
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple (Connection)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Parse
  ( defaultParseRequestBodyOptions
  , lbsBackEnd
  , parseRequestBodyEx
  , setMaxRequestFileSize
  , setMaxRequestFilesSize
  , setMaxRequestKeyLength
  , setMaxRequestNumFiles
  , setMaxRequestParmsSize
  , ParseRequestBodyOptions
  )
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Web.Cookie (parseCookies)

import CMS.Analytics (classifyReferer, isBot, trackable)
import CMS.Auth
import CMS.Blocks (lookupSitePage)
import CMS.DB
import qualified CMS.Handlers.Admin as Admin
import qualified CMS.Handlers.Public as Public
import CMS.Types
import CMS.Upload (serveAsset)
import CMS.Views.Layout (messagePage)
import CMS.Web

mkApp :: Env -> Application
mkApp env req respond =
  case (requestMethod req, pathInfo req) of
    ("GET", ["static", name]) -> respond (serveStatic env name)
    ("GET", ["asset", "img", name]) -> serveAsset env name >>= respond
    _ -> handle env req >>= respond . securityHeaders env

serveStatic :: Env -> Text -> Response
serveStatic env name = case lookup name files of
  Just ct -> responseFile status200 [(hContentType, ct), (hCacheControl, "no-cache")] (envStaticDir env </> T.unpack name) Nothing
  Nothing -> responseLBS status404 [] "not found"
  where
    files =
      [ ("style.css", "text/css; charset=utf-8")
      , ("app.js", "text/javascript; charset=utf-8")
      , ("builder.js", "text/javascript; charset=utf-8")
      ]

-- | Header keamanan untuk semua halaman. Bila situs berjalan di HTTPS (envSecure),
-- semua cookie diberi atribut Secure dan browser diminta selalu memakai HTTPS (HSTS).
-- CSP sengaja tidak membatasi skrip: blok "Kode custom" & Google Analytics butuh skrip
-- inline (blok kode sudah terisolasi di iframe sandbox).
securityHeaders :: Env -> Response -> Response
securityHeaders env =
  mapResponseHeaders $ \hs ->
    [ ("X-Frame-Options", "DENY")
    , ("X-Content-Type-Options", "nosniff")
    , ("Referrer-Policy", "same-origin")
    , ("Content-Security-Policy", "object-src 'none'; base-uri 'self'; frame-ancestors 'none'")
    -- Lokasi boleh diminta halaman situs sendiri (urutan cabang terdekat), tidak oleh iframe lain.
    , ("Permissions-Policy", "camera=(), microphone=(), geolocation=(self), payment=(), usb=()")
    , ("Cross-Origin-Opener-Policy", "same-origin")
    ]
      ++ [("Strict-Transport-Security", "max-age=31536000; includeSubDomains") | envSecure env]
      ++ map secureCookie hs
  where
    secureCookie (k, v)
      | envSecure env && k == "Set-Cookie" && not ("; Secure" `B.isInfixOf` v) = (k, v <> "; Secure")
      | otherwise = (k, v)

handle :: Env -> Request -> IO Response
handle env req = do
  let method = requestMethod req
      cookies = maybe [] parseCookies (lookup hCookie (requestHeaders req))
      cookieText k = TE.decodeUtf8Lenient <$> lookup k cookies
      sessTok = mfilter' (not . T.null) (cookieText "sid")
      decode = map (\(k, v) -> (TE.decodeUtf8Lenient k, maybe "" TE.decodeUtf8Lenient v))
  parsed <-
    if method == methodPost
      then try (parseRequestBodyEx bodyLimits lbsBackEnd req)
      else pure (Right ([], []))
  let (form, files, tooBig) = case parsed of
        Right (ps, fs) ->
          ( map (\(k, v) -> (TE.decodeUtf8Lenient k, TE.decodeUtf8Lenient v)) ps
          , map (\(k, f) -> (TE.decodeUtf8Lenient k, f)) fs
          , False
          )
        Left (_ :: SomeException) -> ([], [], True)
  (mUser, settings) <- withDb env $ \db ->
    (,) <$> maybe (pure Nothing) (sessionUser db . hashToken) sessTok <*> getSettings db
  (csrf, newCsrf) <- case mfilter' (\t -> T.length t == 64) (cookieText "csrf") of
    Just t -> pure (t, False)
    Nothing -> (\t -> (t, True)) <$> randomToken
  let flash = lookup "flash" cookies >>= decodeFlash
      ctx =
        Ctx
          { cEnv = env
          , cReq = req
          , cMethod = method
          , cPath = filter (not . T.null) (pathInfo req)
          , cQuery = decode (queryString req)
          , cForm = form
          , cFiles = files
          , cUser = mUser
          , cSession = sessTok
          , cCsrf = csrf
          , cFlash = flash
          , cSettings = settings
          }
  result <-
    if tooBig
      then
        pure . Right $
          htmlResp status413 (messagePage ctx "Berkas terlalu besar" "Ukuran unggahan melebihi batas (maksimal 2 MB per gambar).")
    else if method == methodPost && lookup "_csrf" form /= Just csrf
      then
        pure . Right $
          htmlResp status403 (messagePage ctx "Formulir kedaluwarsa" "Token keamanan tidak valid. Muat ulang halaman lalu coba lagi.")
      else try (route ctx)
  resp <- case result of
    Right r -> pure r
    Left (e :: SomeException) -> do
      hPutStrLn stderr ("[error] " <> show e)
      pure $ htmlResp status500 (messagePage ctx "Terjadi kesalahan" "Maaf, terjadi kesalahan pada server.")
  trackView ctx resp
  pure
    . (if newCsrf then addHeader (cookieHeader "csrf" (TE.encodeUtf8 csrf) (Just (30 * 86400))) else id)
    . (if isJust flash && not (setsCookie "flash" resp) then addHeader (clearCookie "flash") else id)
    -- Sesi tidak valid/kedaluwarsa: hapus cookie-nya.
    . (if isJust sessTok && not (isJust mUser) && not (setsCookie "sid" resp) then addHeader (clearCookie "sid") else id)
    $ resp

-- | Batas isi request: 1 gambar maks. ~2 MB, isian form maks. 2 MB (JSON page builder).
bodyLimits :: ParseRequestBodyOptions
bodyLimits =
  setMaxRequestFileSize (2 * 1024 * 1024 + 1024)
    . setMaxRequestFilesSize (2 * 1024 * 1024 + 1024)
    . setMaxRequestNumFiles 1
    . setMaxRequestParmsSize (2 * 1024 * 1024)
    . setMaxRequestKeyLength 64
    $ defaultParseRequestBodyOptions

mfilter' :: (a -> Bool) -> Maybe a -> Maybe a
mfilter' p m = case m of
  Just x | p x -> Just x
  _ -> Nothing

route :: Ctx -> IO Response
route c = case (cMethod c, cPath c) of
  ("GET", []) -> Public.home c
  ("GET", ["sitemap.xml"]) -> Public.sitemap c
  ("GET", ["robots.txt"]) -> Public.robots c
  ("GET", ["post", s]) -> Public.showPost c s
  ("GET", ["category", s]) -> Public.categoryArchive c s
  ("GET", ["login"]) -> Public.loginForm c
  ("POST", ["login"]) -> Public.login c
  ("POST", ["logout"]) -> Public.logout c
  ("GET", ["register"]) -> Public.registerForm c
  ("POST", ["register"]) -> Public.register c
  ("GET", ["blog"]) -> Public.blogPage c
  ("GET", [s]) | Just _ <- lookupSitePage s, s `notElem` ["home", "blog"] -> Public.sitePage c s
  (_, "admin" : rest) -> case cUser c of
    Just u -> Admin.adminRoute rest c u
    Nothing ->
      let next = TE.decodeUtf8Lenient (rawPathInfo (cReq c) <> rawQueryString (cReq c))
       in pure (redirectTo (urlWith "/login" [("next", next)]))
  _ -> Public.notFound c

-- | Catat kunjungan halaman publik (tanpa cookie, tanpa data pribadi). Pengguna yang
-- sedang login, mode edit, dan bot tidak dihitung. Kegagalan tidak mengganggu respons.
trackView :: Ctx -> Response -> IO ()
trackView c resp =
  when shouldCount $ do
    r <- try $ withDb (cEnv c) $ \db -> recordView db path source detail
    case r of
      Left (e :: SomeException) -> hPutStrLn stderr ("[analytics] " <> show e)
      Right () -> pure ()
  where
    req = cReq c
    hdr k = TE.decodeUtf8Lenient <$> lookup k (requestHeaders req)
    isHtml = maybe False ("text/html" `B.isPrefixOf`) (lookup hContentType (responseHeaders resp))
    shouldCount =
      cMethod c == methodGet
        && statusCode (responseStatus resp) == 200
        && isHtml
        && trackable (cPath c)
        && not (isJust (cUser c))
        && lookup "edit" (cQuery c) /= Just "1"
        && not (isBot (maybe "" id (hdr hUserAgent)))
    path = "/" <> T.intercalate "/" (cPath c)
    self = maybe "" TE.decodeUtf8Lenient (requestHeaderHost req)
    (source, detail) = classifyReferer self (hdr hReferer)

-- Seed ----------------------------------------------------------------------------

-- | Buat akun admin, kategori, dan artikel contoh bila database masih kosong.
-- Password admin: dari CMS_ADMIN_PASSWORD (tidak dicetak ke log); bila kosong, di
-- produksi dibuat acak dan ditampilkan sekali, di lokal memakai "admin12345".
seedIfEmpty :: Bool -> Connection -> Maybe Text -> IO ()
seedIfEmpty production db mPw = do
  n <- countUsers db
  when (n == 0) $ do
    generated <- T.take 16 <$> randomToken
    let adminPw = case mPw of
          Just p -> p
          Nothing -> if production then generated else "admin12345"
    h <- hashPassword adminPw
    uid <- createUser db "admin" "admin@example.com" h RAdmin "Administrator" True
    cid <- createCategory db "Umum" "umum" "Kabar dan tulisan umum."
    _ <- createCategory db "Tutorial" "tutorial" "Panduan langkah demi langkah."
    _ <-
      createPost
        db
        uid
        PostInput
          { piTitle = "Selamat datang di DCMS"
          , piSlug = "selamat-datang"
          , piExcerpt = "Artikel pertama Anda. Edit atau hapus dari dashboard."
          , piBody = welcomeBody
          , piStatus = Published
          , piCategory = Just cid
          , piSeo = emptySeo
          }
    putStrLn "[seed] Database baru dibuat."
    putStrLn $ case mPw of
      Just _ -> "[seed] Login admin -> username: admin, password dari CMS_ADMIN_PASSWORD."
      Nothing -> "[seed] Login admin -> username: admin  password: " <> T.unpack adminPw
    putStrLn "[seed] Segera ganti password lewat menu Profil."

welcomeBody :: Text
welcomeBody =
  T.unlines
    [ "CMS ini ditulis **sepenuhnya dengan Haskell**: server WAI/Warp, database SQLite, dan HTML dari Lucid."
    , ""
    , "## Peran pengguna"
    , ""
    , "- **Administrator** — akses penuh, termasuk pengguna dan pengaturan situs."
    , "- **Editor** — kelola semua artikel dan kategori, review tulisan User."
    , "- **User** — tulis artikel sendiri lalu kirim untuk direview."
    , ""
    , "## Menulis"
    , ""
    , "Editor mendukung *Markdown*, lengkap dengan toolbar dan pratinjau langsung."
    , ""
    , "```haskell"
    , "main :: IO ()"
    , "main = putStrLn \"Halo dari Haskell!\""
    , "```"
    , ""
    , "> Selamat menulis!"
    ]
