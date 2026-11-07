-- | Akses database SQLite: skema dan semua query.
module CMS.DB where

import Control.Concurrent.MVar
import Control.Monad (forM_, void, when)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField (toField)

import CMS.Blocks
import CMS.BranchSeed (seedBranches)
import CMS.NavStyle (decodeNav, encodeNav)
import CMS.Types

-- | Koneksi dibungkus MVar supaya aman dipakai banyak thread Warp.
withDb :: Env -> (Connection -> IO a) -> IO a
withDb env = withMVar (envDb env)

openDb :: FilePath -> IO (MVar Connection)
openDb path = do
  conn <- open path
  execute_ conn "PRAGMA foreign_keys = ON"
  _ <- query_ conn "PRAGMA journal_mode = WAL" :: IO [Only Text]
  migrate conn
  seedPages conn
  newMVar conn

migrate :: Connection -> IO ()
migrate conn = do
  forM_ schema (execute_ conn)
  -- Kolom foto profil (database lama belum punya).
  hasAvatar <- count1 conn "SELECT COUNT(*) FROM pragma_table_info('users') WHERE name = 'avatar'" ()
  when (hasAvatar == 0) $
    execute_ conn "ALTER TABLE users ADD COLUMN avatar TEXT NOT NULL DEFAULT ''"
  -- Pilihan widget dashboard per pengguna.
  hasDash <- count1 conn "SELECT COUNT(*) FROM pragma_table_info('users') WHERE name = 'dash_hidden'" ()
  when (hasDash == 0) $
    execute_ conn "ALTER TABLE users ADD COLUMN dash_hidden TEXT NOT NULL DEFAULT ''"
  -- Kolom SEO artikel.
  forM_ [("seo_title", "TEXT NOT NULL DEFAULT ''"), ("seo_desc", "TEXT NOT NULL DEFAULT ''"), ("seo_keyword", "TEXT NOT NULL DEFAULT ''"), ("seo_image", "TEXT NOT NULL DEFAULT ''"), ("seo_noindex", "INTEGER NOT NULL DEFAULT 0")] $ \(col, def) -> do
    has <- count1 conn "SELECT COUNT(*) FROM pragma_table_info('posts') WHERE name = ?" (Only (col :: Text))
    when (has == 0) $ execute_ conn (Query ("ALTER TABLE posts ADD COLUMN " <> col <> " " <> def))
  -- Analitik kunjungan: jumlah per hari, halaman, dan sumber (tanpa data pribadi).
  execute_ conn "CREATE TABLE IF NOT EXISTS page_views (\
                \ day TEXT NOT NULL, path TEXT NOT NULL, source TEXT NOT NULL,\
                \ engine TEXT NOT NULL DEFAULT '', views INTEGER NOT NULL DEFAULT 0,\
                \ PRIMARY KEY (day, path, source, engine))"
  execute_ conn "DELETE FROM page_views WHERE day < date('now', '-400 days')"
  -- Cabang/outlet.
  execute_ conn "CREATE TABLE IF NOT EXISTS branches (\
                \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
                \ name TEXT NOT NULL, city TEXT NOT NULL DEFAULT '', province TEXT NOT NULL DEFAULT '',\
                \ address TEXT NOT NULL DEFAULT '', phone TEXT NOT NULL DEFAULT '', phone2 TEXT NOT NULL DEFAULT '',\
                \ whatsapp TEXT NOT NULL DEFAULT '', hours TEXT NOT NULL DEFAULT '', note TEXT NOT NULL DEFAULT '',\
                \ maps_url TEXT NOT NULL DEFAULT '', maps_query TEXT NOT NULL DEFAULT '',\
                \ sort_order INTEGER NOT NULL DEFAULT 0, active INTEGER NOT NULL DEFAULT 1,\
                \ updated_at TEXT NOT NULL DEFAULT (datetime('now')))"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_branches_order ON branches(sort_order, id)"
  forM_ ["lat", "lng"] $ \col -> do
    has <- count1 conn "SELECT COUNT(*) FROM pragma_table_info('branches') WHERE name = ?" (Only (col :: Text))
    when (has == 0) $ execute_ conn (Query ("ALTER TABLE branches ADD COLUMN " <> col <> " REAL"))
  -- Data awal cabang: sekali saja (ditandai), agar tidak muncul lagi bila sengaja dihapus semua.
  seeded <- count1 conn "SELECT COUNT(*) FROM settings WHERE key = 'branches_seeded'" ()
  when (seeded == 0) $ withTransaction conn $ do
    n <- count1 conn "SELECT COUNT(*) FROM branches" ()
    when (n == 0) $ mapM_ (void . createBranch conn) seedBranches
    execute_ conn "INSERT OR REPLACE INTO settings (key, value) VALUES ('branches_seeded', '1')"
    execute_ conn "INSERT OR REPLACE INTO settings (key, value) VALUES ('branches_geo_v1', '1')"
  -- Cabang hasil data awal versi sebelumnya: isi koordinat & perbaiki tautan peta yang
  -- salah di sumber. Hanya baris yang cocok persis dan belum punya koordinat.
  geoDone <- count1 conn "SELECT COUNT(*) FROM settings WHERE key = 'branches_geo_v1'" ()
  when (geoDone == 0) $ withTransaction conn $ do
    forM_ seedBranches $ \BranchInput {..} ->
      execute
        conn
        "UPDATE branches SET lat = ?, lng = ?,\
        \ maps_url = CASE WHEN ? LIKE 'https://www.google.com/maps/search/%' THEN ? ELSE maps_url END\
        \ WHERE name = ? AND address = ? AND lat IS NULL"
        (biLat, biLng, biMapsUrl, biMapsUrl, biName, biAddress)
    execute_ conn "INSERT OR REPLACE INTO settings (key, value) VALUES ('branches_geo_v1', '1')"
  -- Gambar contoh pindah dari /static ke /asset/img.
  execute_ conn "UPDATE pages SET blocks = REPLACE(blocks, '/static/placeholder.svg', '/asset/img/placeholder.svg')\
                \ WHERE blocks LIKE '%/static/placeholder.svg%'"
  where
    schema =
      [ "CREATE TABLE IF NOT EXISTS users (\
        \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
        \ username TEXT NOT NULL UNIQUE COLLATE NOCASE,\
        \ email TEXT NOT NULL UNIQUE COLLATE NOCASE,\
        \ password_hash TEXT NOT NULL,\
        \ role TEXT NOT NULL CHECK (role IN ('admin','editor','user')),\
        \ display_name TEXT NOT NULL DEFAULT '',\
        \ bio TEXT NOT NULL DEFAULT '',\
        \ active INTEGER NOT NULL DEFAULT 1,\
        \ created_at TEXT NOT NULL DEFAULT (datetime('now')))"
      , "CREATE TABLE IF NOT EXISTS sessions (\
        \ token TEXT PRIMARY KEY,\
        \ user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,\
        \ expires_at TEXT NOT NULL)"
      , "CREATE TABLE IF NOT EXISTS categories (\
        \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
        \ name TEXT NOT NULL,\
        \ slug TEXT NOT NULL UNIQUE,\
        \ description TEXT NOT NULL DEFAULT '')"
      , "CREATE TABLE IF NOT EXISTS posts (\
        \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
        \ title TEXT NOT NULL,\
        \ slug TEXT NOT NULL UNIQUE,\
        \ excerpt TEXT NOT NULL DEFAULT '',\
        \ body TEXT NOT NULL,\
        \ status TEXT NOT NULL CHECK (status IN ('draft','pending','published')),\
        \ author_id INTEGER NOT NULL REFERENCES users(id),\
        \ category_id INTEGER REFERENCES categories(id) ON DELETE SET NULL,\
        \ created_at TEXT NOT NULL DEFAULT (datetime('now')),\
        \ updated_at TEXT NOT NULL DEFAULT (datetime('now')),\
        \ published_at TEXT)"
      , "CREATE INDEX IF NOT EXISTS idx_posts_status ON posts(status, published_at)"
      , "CREATE INDEX IF NOT EXISTS idx_posts_author ON posts(author_id)"
      , "CREATE TABLE IF NOT EXISTS settings (key TEXT PRIMARY KEY, value TEXT NOT NULL)"
      , "CREATE TABLE IF NOT EXISTS pages (\
        \ slug TEXT PRIMARY KEY,\
        \ title TEXT NOT NULL,\
        \ blocks TEXT NOT NULL DEFAULT '[]',\
        \ updated_at TEXT NOT NULL DEFAULT (datetime('now')))"
      ]

count1 :: ToRow q => Connection -> Query -> q -> IO Int
count1 c sql ps = maybe 0 fromOnly . listToMaybe <$> query c sql ps

lastId :: Connection -> IO Int
lastId c = fromIntegral <$> lastInsertRowId c

-- Users ---------------------------------------------------------------------

userSelect :: Query
userSelect =
  "SELECT u.id, u.username, u.email, u.password_hash, u.role, u.display_name,\
  \ u.bio, u.active, u.created_at, u.avatar, u.dash_hidden FROM users u"

countUsers :: Connection -> IO Int
countUsers c = count1 c "SELECT COUNT(*) FROM users" ()

findUserById :: Connection -> Int -> IO (Maybe User)
findUserById c i = listToMaybe <$> query c (userSelect <> " WHERE u.id = ?") (Only i)

findUserByLogin :: Connection -> Text -> IO (Maybe User)
findUserByLogin c login =
  listToMaybe <$> query c (userSelect <> " WHERE u.username = ? OR u.email = ?") (login, login)

listUsers :: Connection -> IO [User]
listUsers c = query_ c (userSelect <> " ORDER BY u.id")

-- | Jumlah artikel per penulis.
postCountsByAuthor :: Connection -> IO [(Int, Int)]
postCountsByAuthor c = query_ c "SELECT author_id, COUNT(*) FROM posts GROUP BY author_id"

createUser :: Connection -> Text -> Text -> Text -> Role -> Text -> Bool -> IO Int
createUser c uname email hash role display active = do
  execute
    c
    "INSERT INTO users (username, email, password_hash, role, display_name, active)\
    \ VALUES (?,?,?,?,?,?)"
    (uname, email, hash, role, display, active)
  lastId c

updateUser :: Connection -> Int -> Text -> Text -> Role -> Text -> Bool -> IO ()
updateUser c uid uname email role display active =
  execute
    c
    "UPDATE users SET username = ?, email = ?, role = ?, display_name = ?, active = ? WHERE id = ?"
    (uname, email, role, display, active, uid)

updateProfile :: Connection -> Int -> Text -> Text -> Text -> IO ()
updateProfile c uid email display bio =
  execute c "UPDATE users SET email = ?, display_name = ?, bio = ? WHERE id = ?" (email, display, bio, uid)

setDashHidden :: Connection -> Int -> Text -> IO ()
setDashHidden c uid v = execute c "UPDATE users SET dash_hidden = ? WHERE id = ?" (v, uid)

setUserAvatar :: Connection -> Int -> Text -> IO ()
setUserAvatar c uid url = execute c "UPDATE users SET avatar = ? WHERE id = ?" (url, uid)

updatePassword :: Connection -> Int -> Text -> IO ()
updatePassword c uid hash = execute c "UPDATE users SET password_hash = ? WHERE id = ?" (hash, uid)

-- | Hapus user; artikelnya dipindahkan ke user lain agar tidak hilang.
deleteUser :: Connection -> Int -> Int -> IO ()
deleteUser c uid reassignTo = withTransaction c $ do
  execute c "UPDATE posts SET author_id = ? WHERE author_id = ?" (reassignTo, uid)
  execute c "DELETE FROM users WHERE id = ?" (Only uid)

usernameTaken :: Connection -> Text -> Int -> IO Bool
usernameTaken c n excl = (> 0) <$> count1 c "SELECT COUNT(*) FROM users WHERE username = ? AND id <> ?" (n, excl)

emailTaken :: Connection -> Text -> Int -> IO Bool
emailTaken c e excl = (> 0) <$> count1 c "SELECT COUNT(*) FROM users WHERE email = ? AND id <> ?" (e, excl)

-- Sessions (token disimpan dalam bentuk hash SHA-256) -------------------------

createSession :: Connection -> Text -> Int -> Int -> IO ()
createSession c tokHash uid days =
  execute
    c
    "INSERT INTO sessions (token, user_id, expires_at) VALUES (?, ?, datetime('now', ?))"
    (tokHash, uid, "+" <> T.pack (show days) <> " days")

sessionUser :: Connection -> Text -> IO (Maybe User)
sessionUser c tokHash =
  listToMaybe
    <$> query
      c
      ( userSelect
          <> " JOIN sessions s ON s.user_id = u.id\
             \ WHERE s.token = ? AND s.expires_at > datetime('now') AND u.active = 1"
      )
      (Only tokHash)

deleteSession :: Connection -> Text -> IO ()
deleteSession c tokHash = execute c "DELETE FROM sessions WHERE token = ?" (Only tokHash)

deleteUserSessions :: Connection -> Int -> IO ()
deleteUserSessions c uid = execute c "DELETE FROM sessions WHERE user_id = ?" (Only uid)

purgeExpiredSessions :: Connection -> IO ()
purgeExpiredSessions c = execute_ c "DELETE FROM sessions WHERE expires_at <= datetime('now')"

-- Categories ------------------------------------------------------------------

catSelect :: Query
catSelect =
  "SELECT c.id, c.name, c.slug, c.description,\
  \ (SELECT COUNT(*) FROM posts p WHERE p.category_id = c.id AND p.status = 'published')\
  \ FROM categories c"

listCategories :: Connection -> IO [Category]
listCategories c = query_ c (catSelect <> " ORDER BY c.name COLLATE NOCASE")

findCategory :: Connection -> Int -> IO (Maybe Category)
findCategory c i = listToMaybe <$> query c (catSelect <> " WHERE c.id = ?") (Only i)

findCategoryBySlug :: Connection -> Text -> IO (Maybe Category)
findCategoryBySlug c s = listToMaybe <$> query c (catSelect <> " WHERE c.slug = ?") (Only s)

createCategory :: Connection -> Text -> Text -> Text -> IO Int
createCategory c name slug desc = do
  execute c "INSERT INTO categories (name, slug, description) VALUES (?,?,?)" (name, slug, desc)
  lastId c

updateCategory :: Connection -> Int -> Text -> Text -> Text -> IO ()
updateCategory c i name slug desc =
  execute c "UPDATE categories SET name = ?, slug = ?, description = ? WHERE id = ?" (name, slug, desc, i)

deleteCategory :: Connection -> Int -> IO ()
deleteCategory c i = execute c "DELETE FROM categories WHERE id = ?" (Only i)

catSlugTaken :: Connection -> Text -> Int -> IO Bool
catSlugTaken c s excl = (> 0) <$> count1 c "SELECT COUNT(*) FROM categories WHERE slug = ? AND id <> ?" (s, excl)

-- Posts -----------------------------------------------------------------------

postSelect :: Text
postSelect =
  "SELECT p.id, p.title, p.slug, p.excerpt, p.body, p.status, p.author_id,\
  \ COALESCE(NULLIF(u.display_name, ''), u.username), p.category_id, c.name, c.slug,\
  \ p.created_at, p.updated_at, p.published_at,\
  \ p.seo_title, p.seo_desc, p.seo_keyword, p.seo_image, p.seo_noindex\
  \ FROM posts p JOIN users u ON u.id = p.author_id\
  \ LEFT JOIN categories c ON c.id = p.category_id"

data PostFilter = PostFilter
  { fAuthor :: Maybe Int
  , fStatus :: Maybe PostStatus
  , fCategory :: Maybe Int
  , fSearch :: Maybe Text
  }

noFilter :: PostFilter
noFilter = PostFilter Nothing Nothing Nothing Nothing

data PostOrder = ByPublished | ByUpdated

-- | Daftar artikel dengan filter + paginasi. Mengembalikan (baris, total).
listPosts :: Connection -> PostFilter -> PostOrder -> Int -> Int -> IO ([Post], Int)
listPosts c f order page per = do
  let clauses =
        catMaybes
          [ (\a -> ("p.author_id = ?", [toField a])) <$> fAuthor f
          , (\s -> ("p.status = ?", [toField s])) <$> fStatus f
          , (\k -> ("p.category_id = ?", [toField k])) <$> fCategory f
          , ( \q ->
                let like = "%" <> q <> "%"
                 in ("(p.title LIKE ? OR p.body LIKE ?)", [toField like, toField like])
            )
              <$> (fSearch f >>= \q -> if T.null q then Nothing else Just q)
          ]
      wh = if null clauses then "" else " WHERE " <> T.intercalate " AND " (map fst clauses)
      ps = concatMap snd clauses
      ord = case order of
        ByPublished -> " ORDER BY p.published_at DESC, p.id DESC"
        ByUpdated -> " ORDER BY p.updated_at DESC, p.id DESC"
      offset = (max 1 page - 1) * per
  total <- count1 c (Query ("SELECT COUNT(*) FROM posts p" <> wh)) ps
  rows <-
    query
      c
      (Query (postSelect <> wh <> ord <> " LIMIT ? OFFSET ?"))
      (ps ++ [toField per, toField offset])
  pure (rows, total)

findPost :: Connection -> Int -> IO (Maybe Post)
findPost c i = listToMaybe <$> query c (Query (postSelect <> " WHERE p.id = ?")) (Only i)

findPublishedPost :: Connection -> Text -> IO (Maybe Post)
findPublishedPost c s =
  listToMaybe <$> query c (Query (postSelect <> " WHERE p.slug = ? AND p.status = 'published'")) (Only s)

createPost :: Connection -> Int -> PostInput -> IO Int
createPost c author PostInput {..} = do
  execute
    c
    "INSERT INTO posts (title, slug, excerpt, body, status, author_id, category_id, published_at)\
    \ VALUES (?,?,?,?,?,?,?, CASE WHEN ? = 'published' THEN datetime('now') END)"
    (piTitle, piSlug, piExcerpt, piBody, piStatus, author, piCategory, piStatus)
  pid <- lastId c
  savePostSeo c pid piSeo
  pure pid

savePostSeo :: Connection -> Int -> PostSeo -> IO ()
savePostSeo c pid PostSeo {..} =
  execute
    c
    "UPDATE posts SET seo_title = ?, seo_desc = ?, seo_keyword = ?, seo_image = ?, seo_noindex = ? WHERE id = ?"
    (seoTitle, seoDesc, seoKeyword, seoImage, seoNoindex, pid)

updatePost :: Connection -> Int -> PostInput -> IO ()
updatePost c pid PostInput {..} = do
  savePostSeo c pid piSeo
  execute
    c
    "UPDATE posts SET title = ?, slug = ?, excerpt = ?, body = ?, status = ?, category_id = ?,\
    \ updated_at = datetime('now'),\
    \ published_at = CASE WHEN ? = 'published' THEN COALESCE(published_at, datetime('now'))\
    \ ELSE published_at END\
    \ WHERE id = ?"
    (piTitle, piSlug, piExcerpt, piBody, piStatus, piCategory, piStatus, pid)

setPostStatus :: Connection -> Int -> PostStatus -> IO ()
setPostStatus c pid st =
  execute
    c
    "UPDATE posts SET status = ?, updated_at = datetime('now'),\
    \ published_at = CASE WHEN ? = 'published' THEN COALESCE(published_at, datetime('now'))\
    \ ELSE published_at END WHERE id = ?"
    (st, st, pid)

deletePost :: Connection -> Int -> IO ()
deletePost c pid = execute c "DELETE FROM posts WHERE id = ?" (Only pid)

postSlugTaken :: Connection -> Text -> Int -> IO Bool
postSlugTaken c s excl = (> 0) <$> count1 c "SELECT COUNT(*) FROM posts WHERE slug = ? AND id <> ?" (s, excl)

-- Statistik dashboard; jika penulis diberikan, hanya artikel miliknya.
getStats :: Connection -> Maybe Int -> IO Stats
getStats c mAuthor = do
  rows <-
    query
      c
      "SELECT status, COUNT(*) FROM posts WHERE (? IS NULL OR author_id = ?) GROUP BY status"
      (mAuthor, mAuthor) ::
      IO [(PostStatus, Int)]
  users <- countUsers c
  cats <- count1 c "SELECT COUNT(*) FROM categories" ()
  let get s = fromMaybe 0 (lookup s rows)
  pure
    Stats
      { stTotal = sum (map snd rows)
      , stPublished = get Published
      , stPending = get Pending
      , stDraft = get Draft
      , stUsers = users
      , stCategories = cats
      }

-- Settings ----------------------------------------------------------------------

getSettings :: Connection -> IO Settings
getSettings c = do
  rows <- query_ c "SELECT key, value FROM settings" :: IO [(Text, Text)]
  let d = defaultSettings
      get k def = fromMaybe def (lookup k rows)
  pure
    Settings
      { setSiteName = get "site_name" (setSiteName d)
      , setTagline = get "tagline" (setTagline d)
      , setAllowRegister = get "allow_register" "1" == "1"
      , setPerPage = case reads (T.unpack (get "per_page" "10")) of
          [(n, "")] | n > 0 -> n
          _ -> setPerPage d
      , setNav = decodeNav (get "nav_style" "")
      , setDescription = get "description" ""
      , setWhatsapp = get "whatsapp" ""
      , setWaMessage = get "wa_message" ""
      , setSocial = [(k, v) | (k, _, _) <- socialKinds, let v = get ("social_" <> k) "", not (T.null v)]
      , setSeo =
          SiteSeo
            { ssGoogleVerify = get "seo_google" ""
            , ssBingVerify = get "seo_bing" ""
            , ssGaId = get "seo_ga" ""
            , ssDefaultImage = get "seo_image" ""
            , ssIndexable = get "seo_index" "1" == "1"
            }
      , setLogo = get "logo" ""
      }

-- | Logo disimpan terpisah dari form Pengaturan (diunggah lewat form sendiri).
saveLogo :: Connection -> Text -> IO ()
saveLogo c url = execute c "INSERT OR REPLACE INTO settings (key, value) VALUES ('logo', ?)" (Only url)

saveSiteSeo :: Connection -> SiteSeo -> IO ()
saveSiteSeo c SiteSeo {..} = withTransaction c $
  forM_
    [ ("seo_google", ssGoogleVerify)
    , ("seo_bing", ssBingVerify)
    , ("seo_ga", ssGaId)
    , ("seo_image", ssDefaultImage)
    , ("seo_index", if ssIndexable then "1" else "0")
    ]
    $ \(k, v) -> execute c "INSERT OR REPLACE INTO settings (key, value) VALUES (?, ?)" (k :: Text, v)

-- | Disimpan terpisah dari form Pengaturan karena diatur dari page builder.
saveNavStyle :: Connection -> NavStyle -> IO ()
saveNavStyle c s =
  execute c "INSERT OR REPLACE INTO settings (key, value) VALUES ('nav_style', ?)" (Only (encodeNav s))

saveSettings :: Connection -> Settings -> IO ()
saveSettings c Settings {..} = withTransaction c $
  forM_
    ( [ ("site_name", setSiteName)
    , ("tagline", setTagline)
    , ("allow_register", if setAllowRegister then "1" else "0")
    , ("per_page", T.pack (show setPerPage))
    , ("description", setDescription)
    , ("whatsapp", setWhatsapp)
    , ("wa_message", setWaMessage)
    ]
      ++ [("social_" <> k, fromMaybe "" (lookup k setSocial)) | (k, _, _) <- socialKinds]
    )
    $ \(k, v) ->
      void $ execute c "INSERT OR REPLACE INTO settings (key, value) VALUES (?, ?)" (k :: Text, v)

-- Halaman situs (page builder) ------------------------------------------------------

-- | Buat halaman bawaan yang belum ada (tidak menimpa yang sudah diedit).
seedPages :: Connection -> IO ()
seedPages c = do
  forM_ sitePages $ \sp ->
    execute c "INSERT OR IGNORE INTO pages (slug, title, blocks) VALUES (?,?,?)" (spSlug sp, spTitle sp, encodeBlocks (spDefaults sp))
  -- Sekali saja: halaman Branch memakai direktori cabang otomatis (dari menu Cabang)
  -- menggantikan blok "Daftar cabang" yang diisi manual. Judulnya dipertahankan.
  done <- count1 c "SELECT COUNT(*) FROM settings WHERE key = 'branch_page_dir'" ()
  when (done == 0) $ do
    mp <- findPage c "branch"
    forM_ mp $ \pg -> do
      let toDir b
            | bType b == "branches" = b {bType = "branchdir", bFields = M.filterWithKey (\k _ -> k `elem` ["eyebrow", "title", "text"]) (bFields b), bItems = []}
            | otherwise = b
          blocks = map toDir (pageBlocks pg)
          blocks' = if any ((== "branchdir") . bType) blocks then blocks else blocks ++ [Block "branchdir" (M.fromList [("eyebrow", ""), ("title", "Lokasi kami"), ("text", "")]) []]
      savePageBlocks c "branch" blocks'
    execute_ c "INSERT OR REPLACE INTO settings (key, value) VALUES ('branch_page_dir', '1')"

findPage :: Connection -> Text -> IO (Maybe Page)
findPage c slug = listToMaybe <$> query c "SELECT slug, title, blocks, updated_at FROM pages WHERE slug = ?" (Only slug)

-- | Halaman dalam urutan menu situs.
listPages :: Connection -> IO [Page]
listPages c = do
  ps <- query_ c "SELECT slug, title, blocks, updated_at FROM pages"
  pure [p | sp <- sitePages, p <- ps, pageSlug p == spSlug sp]

savePageBlocks :: Connection -> Text -> [Block] -> IO ()
savePageBlocks c slug bs =
  execute c "UPDATE pages SET blocks = ?, updated_at = datetime('now') WHERE slug = ?" (encodeBlocks bs, slug)

-- Kalender dashboard ------------------------------------------------------------------

-- | Artikel terbit di antara dua tanggal lokal (YYYY-MM-DD, inklusif):
-- (id, judul, slug, tanggal lokal, nama penulis, id penulis).
publishedBetween :: Connection -> Text -> Text -> IO [(Int, Text, Text, Text, Text, Int)]
publishedBetween c from to =
  query
    c
    "SELECT p.id, p.title, p.slug, date(p.published_at, 'localtime'),\
    \ COALESCE(NULLIF(u.display_name, ''), u.username), p.author_id\
    \ FROM posts p JOIN users u ON u.id = p.author_id\
    \ WHERE p.status = 'published' AND p.published_at IS NOT NULL\
    \ AND date(p.published_at, 'localtime') BETWEEN ? AND ?\
    \ ORDER BY p.published_at"
    (from, to)

countActiveSessions :: Connection -> IO Int
countActiveSessions c = count1 c "SELECT COUNT(*) FROM sessions WHERE expires_at > datetime('now')" ()

-- Analitik kunjungan -----------------------------------------------------------------

-- | Catat satu kunjungan (per hari lokal, halaman, sumber, mesin pencari).
recordView :: Connection -> Text -> Text -> Text -> IO ()
recordView c path source engine =
  execute
    c
    "INSERT INTO page_views (day, path, source, engine, views) VALUES (date('now', 'localtime'), ?, ?, ?, 1)\
    \ ON CONFLICT (day, path, source, engine) DO UPDATE SET views = views + 1"
    (T.take 200 path, source, engine)

-- | Kunjungan per hari & sumber dalam n hari terakhir: (tanggal, sumber, jumlah).
viewsByDay :: Connection -> Int -> IO [(Text, Text, Int)]
viewsByDay c days =
  query
    c
    "SELECT day, source, SUM(views) FROM page_views\
    \ WHERE day > date('now', 'localtime', ?) GROUP BY day, source ORDER BY day"
    (Only ("-" <> T.pack (show days) <> " days"))

-- | Total per sumber dalam n hari terakhir.
viewsBySource :: Connection -> Int -> IO [(Text, Int)]
viewsBySource c days =
  query
    c
    "SELECT source, SUM(views) FROM page_views WHERE day > date('now', 'localtime', ?)\
    \ GROUP BY source ORDER BY 2 DESC"
    (Only ("-" <> T.pack (show days) <> " days"))

-- | Mesin pencari pengirim kunjungan dalam n hari terakhir.
viewsByEngine :: Connection -> Int -> IO [(Text, Int)]
viewsByEngine c days =
  query
    c
    "SELECT engine, SUM(views) FROM page_views WHERE source = 'search' AND day > date('now', 'localtime', ?)\
    \ GROUP BY engine ORDER BY 2 DESC"
    (Only ("-" <> T.pack (show days) <> " days"))

-- | Halaman terpopuler: (path, total kunjungan, dari pencarian).
topPages :: Connection -> Int -> Int -> IO [(Text, Int, Int)]
topPages c days n =
  query
    c
    "SELECT path, SUM(views), SUM(CASE WHEN source = 'search' THEN views ELSE 0 END) FROM page_views\
    \ WHERE day > date('now', 'localtime', ?) GROUP BY path ORDER BY 2 DESC LIMIT ?"
    ("-" <> T.pack (show days) <> " days", n)

-- | Semua artikel untuk audit SEO (terbaru dulu).
allPostsForSeo :: Connection -> IO [Post]
allPostsForSeo c = query_ c (Query (postSelect <> " ORDER BY p.updated_at DESC"))

-- | Artikel terbit untuk sitemap (tanpa yang ditandai noindex).
sitemapPosts :: Connection -> IO [(Text, Text)]
sitemapPosts c =
  query_ c "SELECT slug, date(updated_at) FROM posts WHERE status = 'published' AND seo_noindex = 0 ORDER BY published_at DESC"

-- Cabang ------------------------------------------------------------------------------

branchSelect :: Query
branchSelect =
  "SELECT id, name, city, province, address, phone, phone2, whatsapp, hours, note, maps_url, maps_query,\
  \ sort_order, active, updated_at, lat, lng FROM branches"

-- | Semua cabang (dashboard), urut sesuai urutan tampil.
listBranches :: Connection -> IO [Branch]
listBranches c = query_ c (branchSelect <> " ORDER BY sort_order, id")

-- | Cabang aktif (halaman publik).
listActiveBranches :: Connection -> IO [Branch]
listActiveBranches c = query_ c (branchSelect <> " WHERE active = 1 ORDER BY sort_order, id")

findBranch :: Connection -> Int -> IO (Maybe Branch)
findBranch c i = listToMaybe <$> query c (branchSelect <> " WHERE id = ?") (Only i)

countBranches :: Connection -> IO Int
countBranches c = count1 c "SELECT COUNT(*) FROM branches WHERE active = 1" ()

branchFields :: BranchInput -> [SQLData]
branchFields BranchInput {..} =
  [ toField biName, toField biCity, toField biProvince, toField biAddress, toField biPhone, toField biPhone2
  , toField biWhatsapp, toField biHours, toField biNote, toField biMapsUrl, toField biMapsQuery
  , toField biSort, toField biActive, toField biLat, toField biLng ]

createBranch :: Connection -> BranchInput -> IO Int
createBranch c b = do
  execute
    c
    "INSERT INTO branches (name, city, province, address, phone, phone2, whatsapp, hours, note,\
    \ maps_url, maps_query, sort_order, active, lat, lng) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
    (branchFields b)
  lastId c

updateBranch :: Connection -> Int -> BranchInput -> IO ()
updateBranch c i b =
  execute
    c
    "UPDATE branches SET name = ?, city = ?, province = ?, address = ?, phone = ?, phone2 = ?, whatsapp = ?,\
    \ hours = ?, note = ?, maps_url = ?, maps_query = ?, sort_order = ?, active = ?, lat = ?, lng = ?,\
    \ updated_at = datetime('now') WHERE id = ?"
    (branchFields b ++ [toField i])

deleteBranch :: Connection -> Int -> IO ()
deleteBranch c i = execute c "DELETE FROM branches WHERE id = ?" (Only i)

-- | Urutan berikutnya untuk cabang baru (di paling akhir).
nextBranchSort :: Connection -> IO Int
nextBranchSort c = maybe 10 ((+ 10) . fromOnly) . listToMaybe <$> query_ c "SELECT COALESCE(MAX(sort_order), 0) FROM branches"
