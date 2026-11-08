-- | Handler dashboard: artikel, kategori, pengguna, profil, pengaturan.
module CMS.Handlers.Admin (adminRoute) where

import Control.Monad (void, when)
import Data.Aeson (eitherDecodeStrict, encode, object, (.=))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Aeson.Key as Key
import Data.Time
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
import Database.SQLite.Simple (Connection)
import Network.Wai (Response, responseLBS)

import CMS.Auth
import CMS.Blocks
import CMS.DB
import CMS.Seo (analyze, seoInputOf, seoScore)
import CMS.Views.SeoAdmin (SeoData (..), SeoWidget (..), seoPage)
import CMS.Views.BranchAdmin (branchFormPage, branchesPage)
import CMS.Views.ApiAdmin (ApiPageData (..), apiGatewayPage)
import CMS.Api (ApiConf (..), adKey, apiCatalog, decodeApiConf, defaultRateLimit, encodeApiConf)
import CMS.Handlers.Api (newApiKey)
import qualified Data.Map.Strict as M
import CMS.NavStyle (sanitizeNav)
import CMS.Handlers.Public (forbidden, notFound, passwordErrors, validateAccount, validEmail)
import CMS.Status (getServerStatus, statusPairs)
import CMS.Types
import CMS.Upload (removeAsset, saveImage)
import CMS.Util
import CMS.Views.Admin
import CMS.Web

type Handler = Ctx -> User -> IO Response

adminRoute :: [Text] -> Handler
adminRoute path c u = case (cMethod c, path) of
  ("GET", []) -> dashboard c u
  ("GET", ["status"]) -> admin (statusJson c)
  ("POST", ["dashboard", "widgets"]) -> widgetsSave c u
  -- Artikel (semua peran; User hanya miliknya sendiri)
  ("GET", ["posts"]) -> postsIndex c u
  ("GET", ["posts", "new"]) -> postNew c u
  ("POST", ["posts"]) -> postCreate c u
  ("GET", ["posts", i, "edit"]) | Just pid <- readInt i -> withPost pid postEdit
  ("POST", ["posts", i]) | Just pid <- readInt i -> withPost pid postUpdate
  ("POST", ["posts", i, "delete"]) | Just pid <- readInt i -> withPost pid postDelete
  ("POST", ["posts", i, "publish"]) | Just pid <- readInt i -> staff (withPost pid postPublish)
  ("POST", ["preview"]) -> preview c
  -- Halaman situs / page builder (Admin & Editor)
  ("GET", ["pages"]) -> staff (pagesIndex c u)
  ("POST", ["pages", s]) -> staff (pageSave c s)
  -- Kategori (Admin & Editor)
  ("GET", ["categories"]) -> staff (categoriesIndex c u [] ("", "", ""))
  ("POST", ["categories"]) -> staff (categoryCreate c u)
  ("GET", ["categories", i, "edit"]) | Just k <- readInt i -> staff (withCategory k categoryEdit)
  ("POST", ["categories", i]) | Just k <- readInt i -> staff (withCategory k categoryUpdate)
  ("POST", ["categories", i, "delete"]) | Just k <- readInt i -> staff (withCategory k categoryDelete)
  -- Pengguna (Admin)
  ("GET", ["users"]) -> admin (usersIndex c u)
  ("GET", ["users", "new"]) -> admin (ok (userFormPage c u Nothing emptyUserForm []))
  ("POST", ["users"]) -> admin (userCreate c u)
  ("GET", ["users", i, "edit"]) | Just k <- readInt i -> admin (withUser k (\c' u' x -> ok (userFormPage c' u' (Just x) (userToForm x) [])))
  ("POST", ["users", i]) | Just k <- readInt i -> admin (withUser k userUpdate)
  ("POST", ["users", i, "delete"]) | Just k <- readInt i -> admin (withUser k userDelete)
  -- Profil (semua peran)
  ("GET", ["profile"]) -> ok (profilePage c u [] [])
  ("POST", ["profile"]) -> profileUpdate c u
  ("POST", ["profile", "password"]) -> passwordUpdate c u
  ("POST", ["profile", "avatar"]) -> avatarUpload c u
  ("POST", ["profile", "avatar", "delete"]) -> avatarDelete c u
  -- API Gateway (Admin)
  ("GET", ["api"]) -> admin (apiGateway c u Nothing [])
  ("POST", ["api", "config"]) -> admin (apiConfigSave c u)
  ("POST", ["api", "keys"]) -> admin (apiKeyCreate c u)
  ("POST", ["api", "keys", i, "toggle"]) | Just k <- readInt i -> admin (apiKeyToggle c k)
  ("POST", ["api", "keys", i, "delete"]) | Just k <- readInt i -> admin (apiKeyDelete c k)
  -- Cabang (Admin & Editor)
  ("GET", ["branches"]) -> staff (branchesIndex c u)
  ("GET", ["branches", "new"]) -> staff (branchNew c u)
  ("POST", ["branches"]) -> staff (branchSave c u Nothing)
  ("GET", ["branches", i, "edit"]) | Just k <- readInt i -> staff (withBranch k (\b -> ok (branchFormPage c u (Just b) (branchToInput b) [])))
  ("POST", ["branches", i]) | Just k <- readInt i -> staff (withBranch k (branchSave c u . Just))
  ("POST", ["branches", i, "delete"]) | Just k <- readInt i -> staff (withBranch k (branchDelete c))
  -- SEO & analitik (Admin & Editor; pengaturan integrasi khusus Admin)
  ("GET", ["seo"]) -> staff (seoIndex c u [])
  ("POST", ["seo", "settings"]) -> admin (seoSettingsSave c u)
  -- Unggah gambar untuk page builder (Admin & Editor)
  ("POST", ["upload"]) -> staff (imageUpload c)
  -- Pengaturan (Admin)
  ("GET", ["settings"]) -> admin (ok (settingsPage c u (cSettings c) []))
  ("POST", ["settings"]) -> admin (settingsUpdate c u)
  ("POST", ["settings", "logo"]) -> admin (logoUpload c)
  ("POST", ["settings", "logo", "delete"]) -> admin (logoDelete c)
  _ -> notFound c
  where
    staff act = if isStaff u then act else forbidden c
    admin act = if isAdmin u then act else forbidden c
    db :: (Connection -> IO a) -> IO a
    db = withDb (cEnv c)
    withPost pid k =
      db (`findPost` pid) >>= \case
        Just p | canEditPost u p -> k c u p
        Just _ -> forbidden c
        Nothing -> notFound c
    withCategory i k = db (`findCategory` i) >>= maybe (notFound c) (k c u)
    withUser i k = db (`findUserById` i) >>= maybe (notFound c) (k c u)
    withBranch i k = db (`findBranch` i) >>= maybe (notFound c) k


-- Dashboard ---------------------------------------------------------------------

dashboard :: Handler
dashboard c u = do
  let mine = if isStaff u then Nothing else Just (userId u)
  (stats, recent, pending) <- withDb (cEnv c) $ \db -> do
    s <- getStats db mine
    (r, _) <- listPosts db noFilter {fAuthor = mine} ByUpdated 1 6
    (p, _) <-
      if isStaff u
        then listPosts db noFilter {fStatus = Just Pending} ByUpdated 1 6
        else pure ([], 0)
    pure (s, r, p)
  today <- localDay . zonedTimeToLocalTime <$> getZonedTime
  let (ty, tm, _) = toGregorian today
      (y, m) = fromMaybe (ty, tm) (parseMonth (paramT c "bulan"))
      first = fromGregorian y m 1
      lastDay = fromGregorian y m 31 -- otomatis dipotong ke akhir bulan
      inMonth d = let (dy, dm, _) = toGregorian d in dy == y && dm == m
      sel = fromMaybe (if inMonth today then today else first) $ do
        d <- parseDay (paramT c "tgl")
        if inMonth d then Just d else Nothing
  cal <- withDb (cEnv c) $ \db -> publishedBetween db (T.pack (showGregorian first)) (T.pack (showGregorian lastDay))
  status <- if isAdmin u then Just <$> getServerStatus (cEnv c) else pure Nothing
  seoW <-
    if isStaff u
      then withDb (cEnv c) $ \db -> do
        days <- dailyViews db today 14
        posts <- allPostsForSeo db
        pure (Just (SeoWidget days [seoScore (analyze (seoInputOf p)) | p <- posts]))
      else pure Nothing
  ok (dashboardPage c u stats recent pending (CalendarData today y m sel cal) status seoW)
  where
    parseMonth t = case map readInt (T.splitOn "-" t) of
      [Just yy, Just mm] | mm >= 1 && mm <= 12 && yy >= 1970 && yy <= 9999 -> Just (fromIntegral yy, mm)
      _ -> Nothing
    parseDay t = case map readInt (T.splitOn "-" t) of
      [Just yy, Just mm, Just dd] -> fromGregorianValid (fromIntegral yy) mm dd
      _ -> Nothing

-- | Simpan widget dashboard yang disembunyikan. Dari JS: hidden=a,b&ajax=1.
-- Tanpa JS: centang "show" untuk widget yang ditampilkan.
widgetsSave :: Handler
widgetsSave c u = do
  let allowed = map fst (dashWidgets u)
      fromJs = filter (`elem` allowed) . map T.strip . T.splitOn "," <$> lookup "hidden" (cForm c)
      shown = [v | ("show", v) <- cForm c]
      hiddenKs = fromMaybe (filter (`notElem` shown) allowed) fromJs
  withDb (cEnv c) $ \db -> setDashHidden db (userId u) (T.intercalate "," hiddenKs)
  if paramT c "ajax" == "1"
    then pure $ responseLBS status200 [(hContentType, "application/json")] (encode (object ["ok" .= True]))
    else redirectFlash "/admin" "success" "Tampilan dashboard disimpan."

-- | JSON status server untuk pembaruan otomatis di dashboard (Administrator).
statusJson :: Ctx -> IO Response
statusJson c = do
  s <- getServerStatus (cEnv c)
  pure $
    responseLBS status200 [(hContentType, "application/json"), (hCacheControl, "no-store")] $
      encode (object [Key.fromText k .= v | (k, v) <- statusPairs s])

-- Artikel -----------------------------------------------------------------------

postsIndex :: Handler
postsIndex c u = do
  let q = paramT c "q"
      st = paramT c "status"
      page = pageParam c
      per = 15
      flt =
        noFilter
          { fAuthor = if isStaff u then Nothing else Just (userId u)
          , fStatus = statusFromText st
          , fSearch = nonEmpty q
          }
  (posts, total) <- withDb (cEnv c) $ \db -> listPosts db flt ByUpdated page per
  ok (postsIndexPage c u posts total page (totalPages total per) q st)

postNew :: Handler
postNew c u = do
  cats <- withDb (cEnv c) listCategories
  ok (postFormPage c u cats Nothing emptyPostInput [])

-- | Baca form artikel; status dibatasi sesuai peran.
readPostInput :: Ctx -> User -> PostInput
readPostInput c u =
  PostInput
    { piTitle = paramT c "title"
    , piSlug = paramT c "slug"
    , piExcerpt = paramT c "excerpt"
    , piBody = T.replace "\r\n" "\n" (paramRaw c "body")
    , piStatus = clamp (fromMaybe Draft (statusFromText (paramT c "status")))
    , piCategory = paramInt c "category_id"
    , piSeo =
        PostSeo
          { seoTitle = T.take 120 (paramT c "seo_title")
          , seoDesc = T.take 320 (T.unwords (T.words (paramT c "seo_desc")))
          , seoKeyword = T.take 80 (paramT c "seo_keyword")
          , seoImage = safeUrl (paramT c "seo_image")
          , seoNoindex = paramBool c "seo_noindex"
          }
    }
  where
    clamp s = if s `elem` allowedStatuses u then s else Pending

validatePost :: [Category] -> PostInput -> [Text]
validatePost cats PostInput {..} =
  concat
    [ ["Judul wajib diisi." | isBlank piTitle]
    , ["Judul maksimal 200 karakter." | T.length piTitle > 200]
    , ["Isi artikel wajib diisi." | isBlank piBody]
    , ["Kategori tidak valid." | Just k <- [piCategory], k `notElem` map catId cats]
    , ["URL gambar pratinjau harus diawali https:// atau /." | not (isBlank (seoImage piSeo)) && T.null (safeUrl (seoImage piSeo))]
    ]

savePost :: Ctx -> User -> Maybe Post -> IO Response
savePost c u mPost = do
  cats <- withDb (cEnv c) listCategories
  let input = readPostInput c u
      errs = validatePost cats input
      excl = maybe 0 postId mPost
  if not (null errs)
    then pure $ htmlResp status422 (postFormPage c u cats mPost input errs)
    else do
      pid <- withDb (cEnv c) $ \db -> do
        let base = slugify (if isBlank (piSlug input) then piTitle input else piSlug input)
        slug <- uniqueSlug (\s -> postSlugTaken db s excl) base
        let input' = input {piSlug = slug}
        case mPost of
          Nothing -> createPost db (userId u) input'
          Just p -> updatePost db (postId p) input' >> pure (postId p)
      let msg = case piStatus input of
            Published -> "Artikel disimpan dan diterbitkan."
            Pending -> "Artikel disimpan dan dikirim untuk review."
            Draft -> "Draf disimpan."
      redirectFlash ("/admin/posts/" <> tshow pid <> "/edit") "success" msg

postCreate :: Handler
postCreate c u = savePost c u Nothing

postEdit :: Ctx -> User -> Post -> IO Response
postEdit c u p = do
  cats <- withDb (cEnv c) listCategories
  ok (postFormPage c u cats (Just p) (postToInput p) [])

postUpdate :: Ctx -> User -> Post -> IO Response
postUpdate c u p = savePost c u (Just p)

postDelete :: Ctx -> User -> Post -> IO Response
postDelete c _ p = do
  withDb (cEnv c) (`deletePost` postId p)
  redirectFlash "/admin/posts" "success" ("Artikel “" <> postTitle p <> "” dihapus.")

postPublish :: Ctx -> User -> Post -> IO Response
postPublish c _ p = do
  withDb (cEnv c) $ \db -> setPostStatus db (postId p) Published
  redirectFlash "/admin" "success" ("“" <> postTitle p <> "” diterbitkan.")

-- | Pratinjau markdown untuk editor (dipanggil via fetch).
preview :: Ctx -> IO Response
preview c =
  pure $
    responseLBS
      status200
      [(hContentType, "text/html; charset=utf-8")]
      (BL.fromStrict (TE.encodeUtf8 (renderMarkdown (paramRaw c "body"))))

-- Kategori --------------------------------------------------------------------------

type CatInput = (Text, Text, Text)

readCat :: Ctx -> CatInput
readCat c = (paramT c "name", paramT c "slug", paramT c "description")

categoriesIndex :: Ctx -> User -> [Text] -> CatInput -> IO Response
categoriesIndex c u errs input = do
  cats <- withDb (cEnv c) listCategories
  pure $ htmlResp (if null errs then status200 else status422) (categoriesPage c u cats errs input)

-- | Validasi + simpan kategori. Mengembalikan error atau Nothing bila sukses.
saveCategory :: Ctx -> Int -> CatInput -> IO (Maybe [Text])
saveCategory c excl (name, slugIn, desc) = withDb (cEnv c) $ \db -> do
  let slug = slugify (if isBlank slugIn then name else slugIn)
  taken <- catSlugTaken db slug excl
  let errs =
        ["Nama kategori wajib diisi." | isBlank name]
          ++ ["Slug “" <> slug <> "” sudah dipakai." | taken]
  if not (null errs)
    then pure (Just errs)
    else do
      if excl == 0
        then () <$ createCategory db name slug desc
        else updateCategory db excl name slug desc
      pure Nothing

categoryCreate :: Handler
categoryCreate c u = do
  let input = readCat c
  saveCategory c 0 input >>= \case
    Just errs -> categoriesIndex c u errs input
    Nothing -> redirectFlash "/admin/categories" "success" "Kategori ditambahkan."

categoryEdit :: Ctx -> User -> Category -> IO Response
categoryEdit c u k = ok (categoryEditPage c u k [] (catName k, catSlug k, catDesc k))

categoryUpdate :: Ctx -> User -> Category -> IO Response
categoryUpdate c u k = do
  let input = readCat c
  saveCategory c (catId k) input >>= \case
    Just errs -> pure $ htmlResp status422 (categoryEditPage c u k errs input)
    Nothing -> redirectFlash "/admin/categories" "success" "Kategori diperbarui."

categoryDelete :: Ctx -> User -> Category -> IO Response
categoryDelete c _ k = do
  withDb (cEnv c) (`deleteCategory` catId k)
  redirectFlash "/admin/categories" "success" ("Kategori “" <> catName k <> "” dihapus.")

-- Pengguna --------------------------------------------------------------------------

usersIndex :: Handler
usersIndex c u = do
  (users, counts) <- withDb (cEnv c) $ \db -> (,) <$> listUsers db <*> postCountsByAuthor db
  ok (usersPage c u users counts)

readUserForm :: Ctx -> UserForm
readUserForm c =
  UserForm
    { ufUsername = paramT c "username"
    , ufEmail = T.toLower (paramT c "email")
    , ufDisplay = paramT c "display_name"
    , ufRole = fromMaybe RUser (roleFromText (paramT c "role"))
    , ufActive = paramBool c "active"
    }

userCreate :: Handler
userCreate c u = do
  let f@UserForm {..} = readUserForm c
      pw = paramRaw c "password"
  errs <- withDb (cEnv c) $ \db -> validateAccount db 0 ufUsername ufEmail (Just (pw, pw))
  if not (null errs)
    then pure $ htmlResp status422 (userFormPage c u Nothing f errs)
    else do
      h <- hashPassword pw
      _ <- withDb (cEnv c) $ \db -> createUser db ufUsername ufEmail h ufRole ufDisplay ufActive
      redirectFlash "/admin/users" "success" ("Pengguna @" <> ufUsername <> " ditambahkan.")

userUpdate :: Ctx -> User -> User -> IO Response
userUpdate c u x = do
  let f@UserForm {..} = readUserForm c
      pw = paramRaw c "password"
      self = userId x == userId u
      selfErrs =
        ["Anda tidak bisa menurunkan peran akun Anda sendiri." | self && ufRole /= RAdmin]
          ++ ["Anda tidak bisa menonaktifkan akun Anda sendiri." | self && not ufActive]
  errs <-
    (selfErrs ++)
      <$> withDb (cEnv c) (\db -> validateAccount db (userId x) ufUsername ufEmail (if T.null pw then Nothing else Just (pw, pw)))
  if not (null errs)
    then pure $ htmlResp status422 (userFormPage c u (Just x) f errs)
    else do
      mh <- if T.null pw then pure Nothing else Just <$> hashPassword pw
      withDb (cEnv c) $ \db -> do
        updateUser db (userId x) ufUsername ufEmail ufRole ufDisplay ufActive
        mapM_ (updatePassword db (userId x)) mh
        -- Paksa login ulang bila hak akses atau password berubah.
        when (not self && (ufRole /= userRole x || not ufActive || mh /= Nothing)) $
          deleteUserSessions db (userId x)
      redirectFlash "/admin/users" "success" ("Pengguna @" <> ufUsername <> " diperbarui.")

userDelete :: Ctx -> User -> User -> IO Response
userDelete c u x
  | userId x == userId u = redirectFlash "/admin/users" "error" "Anda tidak bisa menghapus akun Anda sendiri."
  | otherwise = do
      withDb (cEnv c) $ \db -> deleteUser db (userId x) (userId u)
      removeAsset (cEnv c) (userAvatar x)
      redirectFlash "/admin/users" "success" ("Pengguna @" <> userName x <> " dihapus.")

-- Profil ------------------------------------------------------------------------------

profileUpdate :: Handler
profileUpdate c u = do
  let email = T.toLower (paramT c "email")
      display = paramT c "display_name"
      bio = paramT c "bio"
  taken <- withDb (cEnv c) $ \db -> emailTaken db email (userId u)
  let errs = ["Email tidak valid." | not (validEmail email)] ++ ["Email sudah dipakai." | taken]
  if not (null errs)
    then pure $ htmlResp status422 (profilePage c u {userEmail = email, userDisplay = display, userBio = bio} errs [])
    else do
      withDb (cEnv c) $ \db -> updateProfile db (userId u) email display bio
      redirectFlash "/admin/profile" "success" "Profil diperbarui."

passwordUpdate :: Handler
passwordUpdate c u = do
  let cur = paramRaw c "current"
      pw = paramRaw c "password"
      pw2 = paramRaw c "password2"
      errs = ["Password saat ini salah." | not (checkPassword cur (userHash u))] ++ passwordErrors pw pw2
  if not (null errs)
    then pure $ htmlResp status422 (profilePage c u [] errs)
    else do
      h <- hashPassword pw
      tok <- randomToken
      withDb (cEnv c) $ \db -> do
        updatePassword db (userId u) h
        deleteUserSessions db (userId u)
        createSession db (hashToken tok) (userId u) sessionDays
      pure
        . addHeader (cookieHeader "sid" (TE.encodeUtf8 tok) (Just (fromIntegral (sessionDays * 86400))))
        . withFlash "success" "Password diganti. Sesi di perangkat lain telah dikeluarkan."
        $ redirectTo "/admin/profile"

-- Pengaturan ----------------------------------------------------------------------------

settingsUpdate :: Handler
settingsUpdate c u = do
  let s =
        Settings
          { setSiteName = paramT c "site_name"
          , setTagline = paramT c "tagline"
          , setAllowRegister = paramBool c "allow_register"
          , setPerPage = fromMaybe 10 (paramInt c "per_page")
          , setNav = setNav (cSettings c)
          , setDescription = T.take 300 (paramT c "description")
          , setWhatsapp = normalizeWa (paramT c "whatsapp")
          , setWaMessage = T.take 300 (paramT c "wa_message")
          , setSocial = [(k, v) | (k, _, _) <- socialKinds, let v = paramT c ("social_" <> k), not (T.null v)]
          , setSeo = setSeo (cSettings c)
          , setLogo = setLogo (cSettings c)
          }
      waLen = T.length (setWhatsapp s)
      badSocial = [lbl | (k, lbl, _) <- socialKinds, Just v <- [lookup k (setSocial s)], not (isWebUrl v)]
      errs =
        ["Nama situs wajib diisi." | isBlank (setSiteName s)]
          ++ ["Artikel per halaman harus 1–100." | setPerPage s < 1 || setPerPage s > 100]
          ++ ["Nomor WhatsApp tidak valid (8–15 digit, mis. 0812xxxx atau 62812xxxx)." | waLen > 0 && (waLen < 8 || waLen > 15)]
          ++ ["Tautan " <> lbl <> " harus diawali https://" | lbl <- badSocial]
  if not (null errs)
    then pure $ htmlResp status422 (settingsPage c u s errs)
    else do
      withDb (cEnv c) (`saveSettings` s)
      redirectFlash "/admin/settings" "success" "Pengaturan disimpan."

-- Halaman situs (page builder) -----------------------------------------------------------

pagesIndex :: Handler
pagesIndex c u = withDb (cEnv c) listPages >>= ok . pagesPage c u

-- | Simpan susunan blok dari editor di halaman, beserta pengaturan navigasi bila
-- dikirim (isian "nav"). Dipanggil lewat fetch(); balas JSON.
pageSave :: Ctx -> Text -> IO Response
pageSave c slug = case lookupSitePage slug of
  Nothing -> notFound c
  Just _ -> case (eitherDecodeStrict (TE.encodeUtf8 (paramRaw c "blocks")), navParam) of
    (Left _, _) -> pure (textResp status400 "Data halaman tidak valid.")
    (_, Left _) -> pure (textResp status400 "Data navigasi tidak valid.")
    (Right raw, Right mNav) -> case sanitizeBlocks raw of
      Left err -> pure (textResp status400 err)
      Right clean -> do
        withDb (cEnv c) $ \db -> do
          savePageBlocks db slug clean
          mapM_ (saveNavStyle db . sanitizeNav) mNav
        pure $
          responseLBS status200 [(hContentType, "application/json")] $
            encode (object ["ok" .= True, "blocks" .= length clean])
  where
    navParam = case paramRaw c "nav" of
      "" -> Right Nothing
      raw -> Just <$> eitherDecodeStrict (TE.encodeUtf8 raw)

-- Gambar: foto profil & unggahan page builder ----------------------------------------------

avatarUpload :: Handler
avatarUpload c u = case lookup "avatar" (cFiles c) of
  Nothing -> redirectFlash "/admin/profile" "error" "Pilih berkas gambar terlebih dahulu."
  Just fi ->
    saveImage (cEnv c) "avatar" fi >>= \case
      Left err -> redirectFlash "/admin/profile" "error" err
      Right url -> do
        withDb (cEnv c) $ \db -> setUserAvatar db (userId u) url
        removeAsset (cEnv c) (userAvatar u)
        redirectFlash "/admin/profile" "success" "Foto profil diperbarui."

avatarDelete :: Handler
avatarDelete c u = do
  withDb (cEnv c) $ \db -> setUserAvatar db (userId u) ""
  removeAsset (cEnv c) (userAvatar u)
  redirectFlash "/admin/profile" "success" "Foto profil dihapus."

-- | Logo situs (Admin). Logo lama dihapus dari folder aset setelah diganti.
logoUpload :: Ctx -> IO Response
logoUpload c = case lookup "logo" (cFiles c) of
  Nothing -> redirectFlash "/admin/settings" "error" "Pilih berkas logo terlebih dahulu."
  Just fi ->
    saveImage (cEnv c) "logo" fi >>= \case
      Left err -> redirectFlash "/admin/settings" "error" err
      Right url -> do
        withDb (cEnv c) (`saveLogo` url)
        removeAsset (cEnv c) (setLogo (cSettings c))
        redirectFlash "/admin/settings" "success" "Logo situs diperbarui."

logoDelete :: Ctx -> IO Response
logoDelete c = do
  withDb (cEnv c) (`saveLogo` "")
  removeAsset (cEnv c) (setLogo (cSettings c))
  redirectFlash "/admin/settings" "success" "Logo situs dihapus."

-- | Dipanggil lewat fetch() dari editor halaman; balas JSON {"url": "..."}.
imageUpload :: Ctx -> IO Response
imageUpload c = case lookup "image" (cFiles c) of
  Nothing -> pure (textResp status400 "Pilih berkas gambar terlebih dahulu.")
  Just fi ->
    saveImage (cEnv c) "img" fi >>= \case
      Left err -> pure (textResp status400 err)
      Right url ->
        pure $ responseLBS status200 [(hContentType, "application/json")] (encode (object ["url" .= url]))

-- | Nomor WhatsApp → hanya angka, format internasional: "0812-3456 789" → "628123456789".
normalizeWa :: Text -> Text
normalizeWa raw =
  let d = T.filter (`elem` ['0' .. '9']) raw
   in if "0" `T.isPrefixOf` d then "62" <> T.drop 1 d else d

isWebUrl :: Text -> Bool
isWebUrl v =
  any (`T.isPrefixOf` T.toLower v) ["https://", "http://"]
    && not (T.any (`elem` ['"', '<', '>', ' ']) v)
    && T.length v <= 300

-- SEO & analitik ---------------------------------------------------------------------------

-- | Kunjungan per hari selama n hari terakhir (hari tanpa kunjungan = 0):
-- (tanggal, dari mesin pencari, dari sumber lain).
dailyViews :: Connection -> Day -> Int -> IO [(Day, Int, Int)]
dailyViews db today n = do
  rows <- viewsByDay db n
  let days = [addDays (negate (fromIntegral k)) today | k <- [n - 1, n - 2 .. 0]]
      sumFor d p = sum [v | (dt, src, v) <- rows, dt == T.pack (showGregorian d), p src]
  pure [(d, sumFor d (== "search"), sumFor d (/= "search")) | d <- days]

seoIndex :: Ctx -> User -> [Text] -> IO Response
seoIndex c u errs = do
  today <- localDay . zonedTimeToLocalTime <$> getZonedTime
  d <- withDb (cEnv c) $ \db -> do
    days <- dailyViews db today 30
    sources <- viewsBySource db 30
    engines <- viewsByEngine db 30
    top <- topPages db 30 10
    posts <- allPostsForSeo db
    let scored = [(p, ck, seoScore ck) | p <- posts, let ck = analyze (seoInputOf p)]
        lv = paramT c "level"
    pure (SeoData days sources engines top scored (if lv `elem` ["good", "warn", "bad"] then lv else ""))
  pure $ htmlResp (if null errs then status200 else status422) (seoPage c u d errs)

seoSettingsSave :: Handler
seoSettingsSave c u = do
  let ga = T.toUpper (T.strip (paramT c "seo_ga"))
      img = paramT c "seo_image"
      ss =
        SiteSeo
          { ssGoogleVerify = verifyCode (paramT c "seo_google")
          , ssBingVerify = verifyCode (paramT c "seo_bing")
          , ssGaId = ga
          , ssDefaultImage = safeUrl img
          , ssIndexable = paramBool c "seo_index"
          }
      gaOk = T.null ga || ("G-" `T.isPrefixOf` ga && T.length ga <= 22 && T.all (\ch -> isAsciiUpper ch || isDigit ch || ch == '-') ga && T.length ga >= 6)
      errs =
        ["ID Google Analytics harus berbentuk G-XXXXXXXXXX." | not gaOk]
          ++ ["URL gambar pratinjau harus diawali https:// atau /." | not (isBlank img) && T.null (safeUrl img)]
          ++ ["Kode verifikasi Google tidak valid." | not (isBlank (paramT c "seo_google")) && T.null (ssGoogleVerify ss)]
          ++ ["Kode verifikasi Bing tidak valid." | not (isBlank (paramT c "seo_bing")) && T.null (ssBingVerify ss)]
  if not (null errs)
    then seoIndex c {cSettings = (cSettings c) {setSeo = ss}} u errs
    else do
      withDb (cEnv c) (`saveSiteSeo` ss)
      redirectFlash "/admin/seo#integrasi" "success" "Pengaturan SEO disimpan."

-- | Ambil kode verifikasi, baik ditempel sebagai kode saja maupun sebagai tag
-- <meta name="…" content="KODE">. Hanya huruf, angka, "-" dan "_".
verifyCode :: Text -> Text
verifyCode raw =
  let t = T.strip raw
      inner = case T.breakOn "content=" t of
        (_, rest)
          | T.null rest -> t
          | otherwise -> T.takeWhile (`notElem` ['"', '\'']) (T.dropWhile (`elem` ['"', '\'']) (T.drop 8 rest))
      ok ch = isAsciiUpper ch || isAsciiLower ch || isDigit ch || ch == '-' || ch == '_'
   in if T.all ok inner && T.length inner <= 120 then inner else ""

-- Cabang ------------------------------------------------------------------------------------

branchesIndex :: Handler
branchesIndex c u = do
  bs <- withDb (cEnv c) listBranches
  let q = T.toLower (paramT c "q")
      prov = paramT c "prov"
      hay b = T.toLower (T.unwords [brName b, brCity b, brProvince b, brAddress b, brPhone b, brPhone2 b])
      shown = [b | b <- bs, T.null q || q `T.isInfixOf` hay b, T.null prov || brProvince b == prov]
  ok (branchesPage c u bs shown (paramT c "q") prov)

branchNew :: Handler
branchNew c u = do
  n <- withDb (cEnv c) nextBranchSort
  ok (branchFormPage c u Nothing emptyBranchInput {biSort = n} [])

readBranchInput :: Ctx -> BranchInput
readBranchInput c =
  BranchInput
    { biName = T.take 120 (paramT c "name")
    , biCity = T.take 80 (paramT c "city")
    , biProvince = T.take 60 (paramT c "province")
    , biAddress = T.take 400 (T.unwords (T.words (paramT c "address")))
    , biPhone = T.take 30 (paramT c "phone")
    , biPhone2 = T.take 30 (paramT c "phone2")
    , biWhatsapp = normalizeWa (paramT c "whatsapp")
    , biHours = T.take 120 (paramT c "hours")
    , biNote = T.take 80 (paramT c "note")
    , biMapsUrl = T.take 500 (paramT c "maps_url")
    , biMapsQuery = T.take 120 (paramT c "maps_query")
    , biSort = maybe 0 (max 0 . min 100000) (paramInt c "sort")
    , biActive = paramBool c "active"
    , biLat = fst <$> coords
    , biLng = snd <$> coords
    }
  where
    -- Koordinat dari isian "lat, lng"; bila kosong, coba ambil dari tautan Google Maps lengkap.
    coords = case parseCoords (paramT c "coords") of
      Just ll -> Just ll
      Nothing | isBlank (paramT c "coords") -> coordsFromMapsUrl (paramT c "maps_url")
      Nothing -> Nothing

-- | Koordinat dari URL Google Maps: ".../place/...!3d-6.2!4d106.8" atau ".../@-6.2,106.8,17z".
coordsFromMapsUrl :: Text -> Maybe (Double, Double)
coordsFromMapsUrl u
  | (_, r) <- T.breakOn "!3d" u, not (T.null r) =
      let (la, r2) = T.breakOn "!4d" (T.drop 3 r)
       in parseCoords (la <> "," <> T.takeWhile (\ch -> isDigit ch || ch `elem` ['.', '-']) (T.drop 3 r2))
  | (_, r) <- T.breakOn "/@" u, not (T.null r) =
      case T.splitOn "," (T.drop 2 r) of
        (la : lo : _) -> parseCoords (la <> "," <> lo)
        _ -> Nothing
  | otherwise = Nothing

validateBranch :: BranchInput -> [Text]
validateBranch BranchInput {..} =
  concat
    [ ["Nama outlet wajib diisi." | isBlank biName]
    , ["Kota / wilayah wajib diisi." | isBlank biCity]
    , ["Provinsi wajib dipilih." | isBlank biProvince]
    , ["Alamat wajib diisi." | isBlank biAddress]
    , ["Nomor telepon tidak valid (8–15 digit)." | badPhone biPhone]
    , ["Nomor telepon kedua tidak valid (8–15 digit)." | badPhone biPhone2]
    , ["Nomor WhatsApp tidak valid (8–15 digit)." | badPhone biWhatsapp]
    , ["Tautan peta harus diawali https://" | not (T.null biMapsUrl) && not (isWebUrl biMapsUrl)]
    ]
  where
    badPhone t = let n = T.length (T.filter isDigit t) in not (T.null t) && (n < 8 || n > 15)

-- | Koordinat diisi tapi tidak bisa dibaca?
badCoords :: Ctx -> [Text]
badCoords c =
  ["Koordinat tidak valid. Tulis seperti: -6.2088, 106.8456" | not (isBlank raw) && isNothing (parseCoords raw)]
  where
    raw = paramT c "coords"

branchSave :: Ctx -> User -> Maybe Branch -> IO Response
branchSave c u mB = do
  let input = readBranchInput c
      errs = validateBranch input ++ badCoords c
  if not (null errs)
    then pure $ htmlResp status422 (branchFormPage c u mB input errs)
    else do
      withDb (cEnv c) $ \db -> case mB of
        Nothing -> void (createBranch db input)
        Just b -> updateBranch db (brId b) input
      redirectFlash "/admin/branches" "success" ("Cabang “" <> biName input <> "” " <> maybe "ditambahkan." (const "diperbarui.") mB)

branchDelete :: Ctx -> Branch -> IO Response
branchDelete c b = do
  withDb (cEnv c) (`deleteBranch` brId b)
  redirectFlash "/admin/branches" "success" ("Cabang “" <> brName b <> "” dihapus.")

-- API Gateway --------------------------------------------------------------------------------

apiGateway :: Ctx -> User -> Maybe (Text, Text) -> [Text] -> IO Response
apiGateway c u newKey errs = do
  d <- withDb (cEnv c) $ \db -> do
    conf <- decodeApiConf <$> getApiConfig db
    keys <- listApiKeys db
    byApi <- apiUsageByApi db 7
    byKey <- apiUsageByKey db 7
    pure (ApiPageData conf keys byApi byKey newKey)
  pure $ htmlResp (if null errs then status200 else status422) (apiGatewayPage c u d errs)

apiConfigSave :: Handler
apiConfigSave c u = do
  let cors = T.take 2000 (T.strip (paramRaw c "cors"))
      entries = filter (not . T.null) (map T.strip (T.split (`elem` [',', '\n', '\r', ' ']) cors))
      badCors = [e | e <- entries, e /= "*", not (any (`T.isPrefixOf` e) ["https://", "http://"]) || T.any (`elem` ['"', '<', '>']) e]
      conf =
        ApiConf
          (M.fromList [(adKey a, (paramBool c ("enabled_" <> adKey a), paramT c ("access_" <> adKey a) == "public")) | a <- apiCatalog])
          (if null entries then "*" else T.intercalate "\n" entries)
  if not (null badCors)
    then apiGateway c u Nothing ["Domain CORS harus diawali https:// atau http://, atau * untuk semua: " <> T.intercalate ", " (take 3 badCors)]
    else do
      withDb (cEnv c) (`saveApiConfig` encodeApiConf conf)
      redirectFlash "/admin/api" "success" "Pengaturan API disimpan."

apiKeyCreate :: Handler
apiKeyCreate c u = do
  let name = T.take 80 (paramT c "name")
      chosen = [v | ("scope", v) <- cForm c]
      scopes
        | "*" `elem` chosen || null valid = "*"
        | otherwise = T.intercalate "," valid
        where
          valid = [adKey a | a <- apiCatalog, adKey a `elem` chosen]
      limit = maybe defaultRateLimit (max 1 . min 10000) (paramInt c "rate_limit")
  if isBlank name
    then apiGateway c u Nothing ["Nama key wajib diisi."]
    else do
      (full, prefix, h) <- newApiKey
      _ <- withDb (cEnv c) $ \db -> createApiKey db name prefix h scopes limit
      -- Tidak redirect: key lengkap hanya ditampilkan di respons ini.
      apiGateway c u (Just (name, full)) []

apiKeyToggle :: Ctx -> Int -> IO Response
apiKeyToggle c i =
  withDb (cEnv c) (`findApiKey` i) >>= \case
    Nothing -> notFound c
    Just k -> do
      withDb (cEnv c) $ \db -> updateApiKey db i (akName k) (akScopes k) (akRateLimit k) (not (akActive k))
      redirectFlash "/admin/api#keys" "success" ("API key “" <> akName k <> "” " <> (if akActive k then "dinonaktifkan." else "diaktifkan."))

apiKeyDelete :: Ctx -> Int -> IO Response
apiKeyDelete c i =
  withDb (cEnv c) (`findApiKey` i) >>= \case
    Nothing -> notFound c
    Just k -> do
      withDb (cEnv c) (`deleteApiKey` i)
      redirectFlash "/admin/api#keys" "success" ("API key “" <> akName k <> "” dihapus.")
