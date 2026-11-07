-- | Handler situs publik, login, logout, dan pendaftaran.
module CMS.Handlers.Public where

import Control.Monad (void)
import Data.Char (isAlphaNum, isAscii)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
import Database.SQLite.Simple (Connection)
import Network.Wai (Response, responseLBS)
import qualified Data.ByteString.Lazy as BL

import CMS.Auth
import CMS.RateLimit (blockedFor, clearKeys, recordFailure)
import CMS.Blocks (Page (..), SitePage (..), pagePath, postsWanted, sitePages)
import CMS.DB
import CMS.Types
import CMS.Util
import CMS.Views.Layout (messagePage)
import CMS.Views.Public
import CMS.Views.Site
import CMS.Web

notFound :: Ctx -> IO Response
notFound c = pure $ htmlResp status404 (messagePage c "Tidak ditemukan" "Halaman yang Anda cari tidak ada.")

forbidden :: Ctx -> IO Response
forbidden c = pure $ htmlResp status403 (messagePage c "Akses ditolak" "Peran Anda tidak memiliki izin untuk halaman ini.")

-- | Beranda = halaman builder "home". Pencarian lama (/?q=) diarahkan ke Blog.
home :: Ctx -> IO Response
home c
  | not (isBlank (paramT c "q")) = pure (redirectTo (urlWith "/blog" [("q", paramT c "q")]))
  | otherwise = sitePage c "home"

-- | Mode edit hanya untuk Admin/Editor yang membuka halaman dengan ?edit=1.
editMode :: Ctx -> Bool
editMode c = paramT c "edit" == "1" && maybe False isStaff (cUser c)

latestPosts :: Ctx -> Int -> IO [Post]
latestPosts c n
  | n <= 0 = pure []
  | otherwise = fst <$> withDb (cEnv c) (\db -> listPosts db noFilter {fStatus = Just Published} ByPublished 1 n)

-- | Halaman situs dari page builder (Home, About Us, Services, Branch, Contact).
sitePage :: Ctx -> Text -> IO Response
sitePage c slug =
  withDb (cEnv c) (`findPage` slug) >>= \case
    Nothing -> notFound c
    Just pg -> do
      let edit = editMode c
      posts <- latestPosts c (if edit then 6 else postsWanted (pageBlocks pg))
      pd <- pageData c pg edit posts
      ok (pageView c pg edit pd mempty)

-- | Blog: blok builder di atas, lalu daftar artikel + pencarian + paginasi.
blogPage :: Ctx -> IO Response
blogPage c =
  withDb (cEnv c) (`findPage` "blog") >>= \case
    Nothing -> notFound c
    Just pg -> do
      let edit = editMode c
          q = paramT c "q"
          page = pageParam c
          per = setPerPage (cSettings c)
          flt = noFilter {fStatus = Just Published, fSearch = nonEmpty q}
      (posts, total, cats) <- withDb (cEnv c) $ \db -> do
        (ps, t) <- listPosts db flt ByPublished page per
        cs <- listCategories db
        pure (ps, t, cs)
      latest <- latestPosts c (if edit then 6 else postsWanted (pageBlocks pg))
      pd <- pageData c pg edit latest
      ok (pageView c pg edit pd (blogListing edit cats posts q page (totalPages total per)))

-- | Data dinamis untuk blok halaman; cabang hanya diambil bila dipakai.
pageData :: Ctx -> Page -> Bool -> [Post] -> IO PageData
pageData c pg edit posts = do
  bs <- if needsBranches edit pg then withDb (cEnv c) listActiveBranches else pure []
  pure (PageData posts bs (setSiteName (cSettings c)))

categoryArchive :: Ctx -> Text -> IO Response
categoryArchive c slug = do
  mk <- withDb (cEnv c) (`findCategoryBySlug` slug)
  maybe (notFound c) (listing c . Just) mk

listing :: Ctx -> Maybe Category -> IO Response
listing c mCat = do
  let q = paramT c "q"
      page = pageParam c
      per = setPerPage (cSettings c)
      flt = noFilter {fStatus = Just Published, fCategory = catId <$> mCat, fSearch = nonEmpty q}
  (posts, total, cats) <- withDb (cEnv c) $ \db -> do
    (ps, t) <- listPosts db flt ByPublished page per
    cs <- listCategories db
    pure (ps, t, cs)
  ok (listingPage c cats mCat posts q page (totalPages total per))

showPost :: Ctx -> Text -> IO Response
showPost c slug = do
  (mp, cats) <- withDb (cEnv c) $ \db -> (,) <$> findPublishedPost db slug <*> listCategories db
  case mp of
    Nothing -> notFound c
    Just p -> ok (postPage c cats p)

-- Login / logout ----------------------------------------------------------------

loginForm :: Ctx -> IO Response
loginForm c = case cUser c of
  Just _ -> pure (redirectTo "/admin")
  Nothing -> ok (loginPage c [] "" (safeNext (paramT c "next")))

login :: Ctx -> IO Response
login c = do
  let ident = paramT c "login"
      pw = paramRaw c "password"
      next = safeNext (paramT c "next")
      lim = envLimiter (cEnv c)
      ip = clientIp c
      acct = T.toLower (T.take 120 ident)
      -- Maks. 5 salah per (IP, akun), 10 per akun dari mana pun, 30 per IP; per 15 menit.
      rules = [("ipacct:" <> ip <> "|" <> acct, 5), ("acct:" <> acct, 10), ("ip:" <> ip, 30)]
      keys = map fst rules
      bad msg = do
        recordFailure lim keys
        pure $ htmlResp status401 (loginPage c [msg] ident next)
  blockedFor lim rules >>= \case
    Just secs ->
      pure . htmlResp status429 $
        loginPage c ["Terlalu banyak percobaan login yang salah. Coba lagi dalam " <> tshow ((secs + 59) `div` 60) <> " menit."] ident next
    Nothing -> do
      mu <- withDb (cEnv c) (`findUserByLogin` ident)
      case mu of
        Just u
          | checkPassword pw (userHash u) ->
              if not (userActive u)
                then pure $ htmlResp status401 (loginPage c ["Akun Anda dinonaktifkan. Hubungi administrator."] ident next)
                else do
                  clearKeys lim (take 2 keys)
                  tok <- randomToken
                  withDb (cEnv c) $ \db -> do
                    purgeExpiredSessions db
                    mapM_ (deleteSession db . hashToken) (cSession c)
                    createSession db (hashToken tok) (userId u) sessionDays
                  pure
                    . addHeader (cookieHeader "sid" (TE.encodeUtf8 tok) (Just (fromIntegral (sessionDays * 86400))))
                    . withFlash "success" ("Selamat datang, " <> userShownName u <> "!")
                    $ redirectTo next
        Just _ -> bad "Username/email atau password salah."
        Nothing -> do
          -- Tetap lakukan kerja bcrypt agar waktu respons tidak membocorkan user yang ada.
          void (hashPassword pw)
          bad "Username/email atau password salah."

logout :: Ctx -> IO Response
logout c = do
  mapM_ (\t -> withDb (cEnv c) (`deleteSession` hashToken t)) (cSession c)
  pure . addHeader (clearCookie "sid") . withFlash "info" "Anda telah keluar." $ redirectTo "/"

-- Registrasi ------------------------------------------------------------------------

registerForm :: Ctx -> IO Response
registerForm c
  | not (setAllowRegister (cSettings c)) = closed c
  | Just _ <- cUser c = pure (redirectTo "/admin")
  | otherwise = ok (registerPage c [] ("", "", ""))

register :: Ctx -> IO Response
register c
  | not (setAllowRegister (cSettings c)) = closed c
  | otherwise = do
      let uname = paramT c "username"
          email = T.toLower (paramT c "email")
          display = paramT c "display_name"
          pw = paramRaw c "password"
          pw2 = paramRaw c "password2"
      errs <- withDb (cEnv c) $ \db -> validateAccount db 0 uname email (Just (pw, pw2))
      if not (null errs)
        then pure $ htmlResp status422 (registerPage c errs (uname, email, display))
        else do
          h <- hashPassword pw
          void $ withDb (cEnv c) $ \db -> createUser db uname email h RUser display True
          redirectFlash "/login" "success" "Akun berhasil dibuat. Silakan masuk."

closed :: Ctx -> IO Response
closed c = pure $ htmlResp status403 (messagePage c "Pendaftaran ditutup" "Pendaftaran pengguna baru sedang dinonaktifkan.")

-- | Validasi data akun. Dipakai juga oleh form pengguna di admin.
validateAccount :: Connection -> Int -> Text -> Text -> Maybe (Text, Text) -> IO [Text]
validateAccount db uid uname email mPw = do
  uTaken <- usernameTaken db uname uid
  eTaken <- emailTaken db email uid
  pure $
    concat
      [ ["Username harus 3–30 karakter (huruf, angka, garis bawah)." | not (validUsername uname)]
      , ["Username sudah dipakai." | uTaken]
      , ["Email tidak valid." | not (validEmail email)]
      , ["Email sudah dipakai." | eTaken]
      , maybe [] (uncurry passwordErrors) mPw
      ]

passwordErrors :: Text -> Text -> [Text]
passwordErrors pw pw2 =
  ["Password minimal 8 karakter." | T.length pw < 8]
    ++ ["Konfirmasi password tidak sama." | pw /= pw2]

validUsername :: Text -> Bool
validUsername u = T.length u >= 3 && T.length u <= 30 && T.all (\ch -> (isAscii ch && isAlphaNum ch) || ch == '_') u

validEmail :: Text -> Bool
validEmail e = case T.splitOn "@" e of
  [l, d] -> not (T.null l) && "." `T.isInfixOf` d && not (T.any (== ' ') e) && T.length e <= 254
  _ -> False

-- SEO: sitemap & robots ------------------------------------------------------------------

-- | /sitemap.xml: halaman situs, artikel terbit (kecuali noindex), dan kategori.
sitemap :: Ctx -> IO Response
sitemap c = do
  (posts, cats) <- withDb (cEnv c) $ \db -> (,) <$> sitemapPosts db <*> listCategories db
  let origin = siteOrigin c
      url loc lastmod =
        "<url><loc>" <> xmlEsc (origin <> loc) <> "</loc>"
          <> maybe "" (\d -> "<lastmod>" <> xmlEsc d <> "</lastmod>") lastmod
          <> "</url>"
      body =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n"
          <> T.concat [url (pagePath (spSlug sp)) Nothing <> "\n" | sp <- sitePages]
          <> T.concat [url ("/post/" <> s) (Just d) <> "\n" | (s, d) <- posts]
          <> T.concat [url ("/category/" <> catSlug k) Nothing <> "\n" | k <- cats, catCount k > 0]
          <> "</urlset>\n"
  pure $ responseLBS status200 [(hContentType, "application/xml; charset=utf-8")] (BL.fromStrict (TE.encodeUtf8 body))
  where
    xmlEsc = T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "&" "&amp;"

-- | /robots.txt: arahkan perayap ke sitemap; tutup dashboard. Bila pengindeksan
-- dimatikan di menu SEO, seluruh situs ditutup.
robots :: Ctx -> IO Response
robots c = do
  let origin = siteOrigin c
      open = ssIndexable (setSeo (cSettings c))
      body =
        T.unlines $
          ["User-agent: *"]
            ++ (if open then ["Disallow: /admin", "Disallow: /login", "Disallow: /register", "Disallow: /*?edit=1"] else ["Disallow: /"])
            ++ ["", "Sitemap: " <> origin <> "/sitemap.xml"]
  pure $ responseLBS status200 [(hContentType, "text/plain; charset=utf-8")] (BL.fromStrict (TE.encodeUtf8 body))
