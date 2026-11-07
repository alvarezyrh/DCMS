-- | Halaman dashboard admin.
module CMS.Views.Admin where

import Control.Monad (forM_, replicateM_, unless, when)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, DayOfWeek (..), addGregorianMonthsClip, dayOfWeek, fromGregorian, gregorianMonthLength, showGregorian)
import Lucid
import Lucid.Base (makeAttribute)

import CMS.Blocks (Page (..), pagePath)
import CMS.Seo
import CMS.Views.SeoAdmin (SeoWidget, checkItem, scoreRing, seoWidgetCard)
import CMS.Web (siteOrigin)
import CMS.Status (ServerStatus (..))
import CMS.Types
import CMS.Util
import CMS.Views.Layout

-- Dashboard -------------------------------------------------------------------

statCard :: Text -> Int -> Text -> Html ()
statCard label n href =
  a_ [href_ href, class_ "card stat"] $ do
    span_ [class_ "stat-label"] (toHtml label)
    span_ [class_ "stat-value"] (toHtml (show n))

dashboardPage :: Ctx -> User -> Stats -> [Post] -> [Post] -> CalendarData -> Maybe ServerStatus -> Maybe SeoWidget -> Html ()
dashboardPage c u Stats {..} recent pending cal mStatus mSeo = adminLayout c u "dashboard" "Dashboard" $ do
  let hiddenKs = hiddenWidgets u
      available = dashWidgets u
      widget :: Text -> Html () -> Html ()
      widget k inner =
        div_ ([class_ "widget", data_ "widget" k] <> [makeAttribute "hidden" "" | k `elem` hiddenKs]) $ do
          button_ [type_ "button", class_ "widget-hide", data_ "widget-hide" k, title_ "Sembunyikan widget", makeAttribute "aria-label" "Sembunyikan widget"] $
            icon "fa-regular fa-eye-slash"
          inner
  div_ [class_ "dash-toolbar", data_ "dash" ""] $
    details_ [class_ "dash-config"] $ do
      summary_ [class_ "btn btn-ghost btn-sm"] (icon "fa-solid fa-sliders" >> "Atur widget")
      form_ [method_ "post", action_ "/admin/dashboard/widgets", class_ "dash-config-panel", data_ "widget-form" ""] $ do
        csrfField c
        h4_ "Tampilkan widget"
        forM_ available $ \(k, label) ->
          label_ [class_ "switch-row"] $ do
            span_ (toHtml label)
            input_ ([type_ "checkbox", name_ "show", value_ k] <> [checked_ | k `notElem` hiddenKs])
        button_ [type_ "submit", class_ "btn btn-sm btn-block"] "Simpan"
  div_ ([class_ "empty widgets-empty card", data_ "widgets-empty" ""] <> [makeAttribute "hidden" "" | not (all ((`elem` hiddenKs) . fst) available)]) $
    p_ $ do
      "Semua widget disembunyikan. Klik "
      strong_ "Atur widget"
      " di atas untuk memunculkannya kembali."
  widget "welcome" $ section_ [class_ "welcome card"] $ do
    h2_ (toHtml ("Halo, " <> userShownName u <> "!"))
    p_ [class_ "muted"] $ toHtml $ case userRole u of
      RAdmin -> "Anda Administrator: kelola semua konten, kategori, pengguna, dan pengaturan situs." :: Text
      REditor -> "Anda Editor: kelola semua artikel dan kategori, serta review tulisan dari User."
      RUser -> "Anda User: tulis artikel, lalu kirim untuk direview Editor sebelum terbit."
    a_ [href_ "/admin/posts/new", class_ "btn"] "+ Tulis artikel baru"
  widget "stats" $ div_ [class_ "stats"] $ do
    statCard (if isStaff u then "Total artikel" else "Artikel saya") stTotal "/admin/posts"
    statCard "Terbit" stPublished "/admin/posts?status=published"
    statCard "Menunggu review" stPending "/admin/posts?status=pending"
    when (isStaff u) $ statCard "Kategori" stCategories "/admin/categories"
    when (isAdmin u) $ statCard "Pengguna" stUsers "/admin/users"
  forM_ mSeo (widget "seo" . seoWidgetCard)
  div_ [class_ ("dash-row" <> maybe " no-status" (const "") mStatus)] $ do
    widget "calendar" (calendarCard u cal)
    forM_ mStatus (widget "status" . statusCard)
  div_ [class_ "grid-2"] $ do
    when (isStaff u) $
      widget "pending" $ section_ [class_ "card"] $ do
        h3_ "Menunggu review"
        if null pending
          then emptyState "Tidak ada artikel yang menunggu review."
          else ul_ [class_ "list"] $ forM_ pending $ \p -> li_ $ do
            div_ $ do
              a_ [href_ (editUrl p)] (toHtml (postTitle p))
              small_ [class_ "muted"] (toHtml (" · " <> postAuthorName p <> " · " <> fmtDate (postUpdated p)))
            form_ [method_ "post", action_ ("/admin/posts/" <> tshow (postId p) <> "/publish"), class_ "inline"] $ do
              csrfField c
              button_ [type_ "submit", class_ "btn btn-sm"] "Terbitkan"
    widget "recent" $ section_ [class_ "card"] $ do
      h3_ "Terakhir diperbarui"
      if null recent
        then emptyState "Belum ada artikel."
        else ul_ [class_ "list"] $ forM_ recent $ \p -> li_ $ do
          div_ $ do
            a_ [href_ (editUrl p)] (toHtml (postTitle p))
            small_ [class_ "muted"] (toHtml (" · " <> fmtDate (postUpdated p)))
          statusBadge (postStatus p)

-- | Widget dashboard yang tersedia untuk peran ini: (kunci, nama).
dashWidgets :: User -> [(Text, Text)]
dashWidgets u =
  [("welcome", "Sambutan"), ("stats", "Ringkasan angka")]
    ++ [("seo", "SEO & Analitik") | isStaff u]
    ++ [("calendar", "Kalender")]
    ++ [("status", "Status server") | isAdmin u]
    ++ [("pending", "Menunggu review") | isStaff u]
    ++ [("recent", "Terakhir diperbarui")]

hiddenWidgets :: User -> [Text]
hiddenWidgets = filter (not . T.null) . map T.strip . T.splitOn "," . userDashHidden

-- Kalender ----------------------------------------------------------------------------

data CalendarData = CalendarData
  { calToday :: Day
  , calYear :: Integer
  , calMonth :: Int
  , calSelected :: Day
  , calPosts :: [(Int, Text, Text, Text, Text, Int)] -- id, judul, slug, tanggal, penulis, id penulis
  }

calendarCard :: User -> CalendarData -> Html ()
calendarCard u CalendarData {..} =
  section_ [class_ "card cal-card", id_ "kalender"] $ do
    div_ [class_ "cal-head"] $ do
      h3_ $ do
        icon "fa-regular fa-calendar"
        toHtml (" " <> monthName calMonth <> " " <> tshow calYear)
      div_ [class_ "cal-nav"] $ do
        a_ [href_ (monthLink (-1)), class_ "btn btn-ghost btn-sm", makeAttribute "aria-label" "Bulan sebelumnya"] (icon "fa-solid fa-chevron-left")
        a_ [href_ "/admin#kalender", class_ "btn btn-ghost btn-sm"] "Hari ini"
        a_ [href_ (monthLink 1), class_ "btn btn-ghost btn-sm", makeAttribute "aria-label" "Bulan berikutnya"] (icon "fa-solid fa-chevron-right")
    div_ [class_ "cal-grid"] $ do
      forM_ ["Sen", "Sel", "Rab", "Kam", "Jum", "Sab", "Min"] $ \w -> div_ [class_ "cal-wd"] (toHtml (w :: Text))
      replicateM_ (lead first) (div_ [class_ "cal-day is-empty"] "")
      forM_ [1 .. gregorianMonthLength calYear calMonth] $ \d -> do
        let day = fromGregorian calYear calMonth d
            ps = postsOn day
            cls =
              T.unwords $
                ["cal-day"]
                  <> ["is-today" | day == calToday]
                  <> ["is-selected" | day == calSelected]
                  <> ["has-posts" | not (null ps)]
        a_ ([href_ (dayLink day), class_ cls] <> [title_ (T.intercalate " · " [t | (_, t, _, _, _, _) <- ps]) | not (null ps)]) $ do
          span_ [class_ "cal-num"] (toHtml (show d))
          unless (null ps) $
            span_ [class_ "cal-marks"] $ do
              replicateM_ (min 3 (length ps)) (span_ [class_ "cal-dot"] "")
              when (length ps > 3) $ small_ (toHtml ("+" <> tshow (length ps - 3)))
    div_ [class_ "cal-list"] $ do
      h4_ (toHtml ("Terbit " <> fmtDate (T.pack (showGregorian calSelected))))
      let sel = postsOn calSelected
      if null sel
        then p_ [class_ "muted"] "Tidak ada artikel yang terbit pada tanggal ini."
        else ul_ [class_ "list"] $ forM_ sel $ \(pid, title, slug, _, author, aid) -> li_ $ do
          div_ $ do
            a_ [href_ ("/post/" <> slug), target_ "_blank", rel_ "noopener"] (toHtml title)
            small_ [class_ "muted"] (toHtml (" · " <> author))
          when (isStaff u || aid == userId u) $
            a_ [href_ ("/admin/posts/" <> tshow pid <> "/edit"), class_ "btn btn-ghost btn-sm"] "Edit"
    p_ [class_ "cal-legend muted"] $ do
      span_ [class_ "cal-dot"] ""
      " Artikel terbit · "
      span_ [class_ "cal-today-mark"] ""
      " Hari ini"
  where
    first = fromGregorian calYear calMonth 1
    postsOn day = [p | p@(_, _, _, d, _, _) <- calPosts, d == T.pack (showGregorian day)]
    lead d = case dayOfWeek d of
      Monday -> 0
      Tuesday -> 1
      Wednesday -> 2
      Thursday -> 3
      Friday -> 4
      Saturday -> 5
      Sunday -> 6
    ym d = T.take 7 (T.pack (showGregorian d))
    monthLink n = "/admin?bulan=" <> ym (addGregorianMonthsClip n first) <> "#kalender"
    dayLink d = "/admin?bulan=" <> ym d <> "&tgl=" <> T.pack (showGregorian d) <> "#kalender"

monthName :: Int -> Text
monthName m =
  case drop (m - 1) ["Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", "Agustus", "September", "Oktober", "November", "Desember"] of
    (n : _) -> n
    [] -> ""

-- Status server ----------------------------------------------------------------------------

statusCard :: ServerStatus -> Html ()
statusCard ServerStatus {..} =
  section_ [class_ "card status-card", data_ "server-status" ""] $ do
    div_ [class_ "status-head"] $ do
      h3_ (icon "fa-solid fa-server" >> " Status server")
      span_ [class_ ("status-pill " <> if ssDbOk then "ok" else "warn"), data_ "status-pill" ""] $ do
        span_ [class_ "pulse"] ""
        span_ [data_ "status-text" ""] (if ssDbOk then "Online" else "Database bermasalah")
    dl_ [class_ "status-list"] $ do
      row "fa-regular fa-clock" "Uptime" $ do
        span_ [data_ "stat" "uptime"] (toHtml ssUptime)
        small_ [class_ "muted"] $ do
          "sejak "
          span_ [data_ "stat" "started"] (toHtml ssStarted)
      row "fa-solid fa-memory" "Memori" (span_ [data_ "stat" "memory"] (toHtml ssMemory))
      row "fa-solid fa-microchip" "Beban CPU" $ do
        span_ [data_ "stat" "load"] (toHtml ssLoad)
        small_ [class_ "muted", data_ "stat" "cpu"] (toHtml ssCpu)
      row "fa-solid fa-database" "Database" $ do
        span_ [data_ "stat" "db"] (if ssDbOk then "Terhubung" else "Gangguan")
        small_ [class_ "muted", data_ "stat" "dbsize"] (toHtml ssDbSize)
      row "fa-solid fa-user-clock" "Sesi aktif" (span_ [data_ "stat" "sessions"] (toHtml (tshow ssSessions)))
      row "fa-regular fa-images" "Aset gambar" (span_ [data_ "stat" "assets"] (toHtml ssAssets))
      row "fa-solid fa-code" "Runtime" (span_ [data_ "stat" "runtime"] (toHtml ssRuntime))
    p_ [class_ "status-foot muted"] $ do
      "Diperbarui "
      span_ [data_ "stat" "time"] (toHtml ssTime)
      " · otomatis tiap 10 detik"
  where
    row :: Text -> Text -> Html () -> Html ()
    row ic label val = div_ [class_ "status-row"] $ do
      dt_ (icon ic >> toHtml (" " <> label))
      dd_ val

editUrl :: Post -> Text
editUrl p = "/admin/posts/" <> tshow (postId p) <> "/edit"

-- Artikel -----------------------------------------------------------------------

postsIndexPage :: Ctx -> User -> [Post] -> Int -> Int -> Int -> Text -> Text -> Html ()
postsIndexPage c u posts total page pages q st = adminLayout c u "posts" "Artikel" $ do
  div_ [class_ "toolbar"] $ do
    form_ [method_ "get", action_ "/admin/posts", class_ "filters"] $ do
      input_ [type_ "search", name_ "q", value_ q, placeholder_ "Cari judul atau isi…"]
      select_ [name_ "status"] $ do
        option_ [value_ ""] "Semua status"
        forM_ [minBound .. maxBound] $ \s ->
          option_ ([value_ (statusText s)] <> [selected_ "selected" | statusText s == st]) (toHtml (statusLabel s))
      button_ [type_ "submit", class_ "btn btn-ghost"] "Filter"
    a_ [href_ "/admin/posts/new", class_ "btn"] "+ Tulis baru"
  p_ [class_ "muted"] (toHtml (show total <> " artikel"))
  if null posts
    then emptyState "Belum ada artikel di sini."
    else div_ [class_ "table-wrap card"] $ table_ [class_ "table"] $ do
      thead_ $ tr_ $ do
        th_ "Judul"
        when (isStaff u) $ th_ "Penulis"
        th_ "Kategori"
        th_ "Status"
        th_ "Diperbarui"
        th_ [class_ "right"] "Aksi"
      tbody_ $ forM_ posts $ \p -> tr_ $ do
        td_ $ do
          a_ [href_ (editUrl p), class_ "strong"] (toHtml (postTitle p))
          br_ []
          small_ [class_ "muted mono"] (toHtml ("/post/" <> postSlug p))
        when (isStaff u) $ td_ (toHtml (postAuthorName p))
        td_ (toHtml (fromMaybe "—" (postCategoryName p)))
        td_ (statusBadge (postStatus p))
        td_ (toHtml (fmtDate (postUpdated p)))
        td_ [class_ "right actions"] $ do
          when (postStatus p == Published) $
            a_ [href_ ("/post/" <> postSlug p), target_ "_blank", class_ "btn btn-ghost btn-sm"] "Lihat"
          a_ [href_ (editUrl p), class_ "btn btn-ghost btn-sm"] "Edit"
          deleteButton c ("/admin/posts/" <> tshow (postId p) <> "/delete") "Hapus artikel ini?"
  pagination "/admin/posts" [("q", q), ("status", st)] page pages

deleteButton :: Ctx -> Text -> Text -> Html ()
deleteButton c action msg =
  form_ [method_ "post", action_ action, class_ "inline", data_ "confirm" msg] $ do
    csrfField c
    button_ [type_ "submit", class_ "btn btn-danger btn-sm"] "Hapus"

postFormPage :: Ctx -> User -> [Category] -> Maybe Post -> PostInput -> [Text] -> Html ()
postFormPage c u cats mPost PostInput {..} errs =
  adminLayout c u (if isJust mPost then "posts" else "new") (if isJust mPost then "Edit Artikel" else "Tulis Artikel") $ do
    errorList errs
    form_ [method_ "post", action_ action, class_ "editor-layout", id_ "post-form"] $ do
      csrfField c
      div_ [class_ "editor-main"] $ do
        input_
          [ type_ "text", name_ "title", id_ "title", value_ piTitle, class_ "title-input"
          , placeholder_ "Judul artikel", required_ "required", makeAttribute "aria-label" "Judul" ]
        div_ [class_ "slug-row"] $ do
          span_ [class_ "muted mono"] "/post/"
          input_ [type_ "text", name_ "slug", id_ "slug", value_ piSlug, class_ "mono", placeholder_ "otomatis-dari-judul"]
        div_ [class_ "card editor"] $ do
          div_ [class_ "editor-bar"] $ do
            div_ [class_ "tabs"] $ do
              button_ [type_ "button", class_ "tab active", data_ "tab" "write"] "Tulis"
              button_ [type_ "button", class_ "tab", data_ "tab" "preview"] "Pratinjau"
            div_ [class_ "md-tools"] $
              forM_ tools $ \(cmd, label, tip) ->
                button_ [type_ "button", data_ "md" cmd, title_ tip, class_ "tool"] (toHtml label)
          textarea_
            [ name_ "body", id_ "body", rows_ "22", class_ "md-input"
            , placeholder_ "Tulis isi artikel dengan Markdown…", makeAttribute "aria-label" "Isi artikel" ]
            (toHtml piBody)
          div_ [id_ "preview", class_ "prose preview", hidden_ "hidden"] mempty
          div_ [class_ "editor-foot muted"] $ do
            span_ [id_ "wordcount"] "0 kata"
            span_ "Markdown didukung · Ctrl+S untuk simpan"
        textareaField "Ringkasan (opsional)" "excerpt" piExcerpt 3 [placeholder_ "Ditampilkan di daftar artikel. Kosongkan untuk otomatis."]
        seoPanel c PostInput {..}
      aside_ [class_ "editor-side"] $ do
        seoScoreCard (analyze (SeoInput piTitle piSlug piBody piExcerpt piSeo))
        div_ [class_ "card"] $ do
          h3_ "Publikasi"
          selectField "Status" "status" [(statusText s, statusLabel s) | s <- allowedStatuses u] (statusText piStatus)
          unless (isStaff u) $ hint "Pilih “Menunggu review” bila siap diterbitkan Editor."
          selectField "Kategori" "category_id" (("", "— Tanpa kategori —") : [(tshow (catId k), catName k) | k <- cats]) (maybe "" tshow piCategory)
          button_ [type_ "submit", class_ "btn btn-block"] (if isJust mPost then "Simpan perubahan" else "Simpan artikel")
        forM_ mPost $ \p -> div_ [class_ "card meta-card"] $ do
          h3_ "Info"
          dl_ $ do
            dt_ "Penulis" >> dd_ (toHtml (postAuthorName p))
            dt_ "Dibuat" >> dd_ (toHtml (fmtDate (postCreated p)))
            dt_ "Diperbarui" >> dd_ (toHtml (fmtDate (postUpdated p)))
            forM_ (postPublished p) $ \d -> dt_ "Terbit" >> dd_ (toHtml (fmtDate d))
          when (postStatus p == Published) $
            a_ [href_ ("/post/" <> postSlug p), target_ "_blank", class_ "btn btn-ghost btn-block"] "Lihat di situs ↗"
          button_ [type_ "submit", form_ "delete-form", class_ "btn btn-danger btn-block"] "Hapus artikel"
    forM_ mPost $ \p ->
      form_ [method_ "post", action_ ("/admin/posts/" <> tshow (postId p) <> "/delete"), id_ "delete-form", data_ "confirm" "Hapus artikel ini secara permanen?"] $
        csrfField c
  where
    action = maybe "/admin/posts" (\p -> "/admin/posts/" <> tshow (postId p)) mPost
    tools :: [(Text, Text, Text)]
    tools =
      [ ("h2", "H2", "Judul bagian"), ("bold", "B", "Tebal"), ("italic", "I", "Miring")
      , ("link", "🔗", "Tautan"), ("image", "🖼", "Gambar"), ("ul", "•", "Daftar")
      , ("ol", "1.", "Daftar bernomor"), ("quote", "❝", "Kutipan"), ("code", "</>", "Kode") ]

-- Kategori --------------------------------------------------------------------------

categoriesPage :: Ctx -> User -> [Category] -> [Text] -> (Text, Text, Text) -> Html ()
categoriesPage c u cats errs (name, slug, desc) = adminLayout c u "categories" "Kategori" $
  div_ [class_ "grid-side"] $ do
    section_ [class_ "card"] $ do
      h3_ "Tambah kategori"
      errorList errs
      form_ [method_ "post", action_ "/admin/categories", class_ "stack"] $ do
        csrfField c
        categoryFields name slug desc
        button_ [type_ "submit", class_ "btn"] "Tambah"
    section_ [class_ "card table-wrap"] $
      if null cats
        then emptyState "Belum ada kategori."
        else table_ [class_ "table"] $ do
          thead_ $ tr_ $ th_ "Nama" >> th_ "Slug" >> th_ "Artikel terbit" >> th_ [class_ "right"] "Aksi"
          tbody_ $ forM_ cats $ \k -> tr_ $ do
            td_ $ do
              strong_ (toHtml (catName k))
              unless (isBlank (catDesc k)) $ br_ [] >> small_ [class_ "muted"] (toHtml (catDesc k))
            td_ [class_ "mono"] (toHtml (catSlug k))
            td_ (toHtml (show (catCount k)))
            td_ [class_ "right actions"] $ do
              a_ [href_ ("/admin/categories/" <> tshow (catId k) <> "/edit"), class_ "btn btn-ghost btn-sm"] "Edit"
              deleteButton c ("/admin/categories/" <> tshow (catId k) <> "/delete") "Hapus kategori ini? Artikelnya tidak ikut terhapus."

categoryFields :: Text -> Text -> Text -> Html ()
categoryFields name slug desc = do
  inputField "text" "Nama" "name" name [required_ "required"]
  inputField "text" "Slug" "slug" slug [placeholder_ "otomatis dari nama", class_ "mono"]
  textareaField "Deskripsi" "description" desc 3 []

categoryEditPage :: Ctx -> User -> Category -> [Text] -> (Text, Text, Text) -> Html ()
categoryEditPage c u k errs (name, slug, desc) = adminLayout c u "categories" "Edit Kategori" $
  section_ [class_ "card narrow"] $ do
    errorList errs
    form_ [method_ "post", action_ ("/admin/categories/" <> tshow (catId k)), class_ "stack"] $ do
      csrfField c
      categoryFields name slug desc
      div_ [class_ "row"] $ do
        button_ [type_ "submit", class_ "btn"] "Simpan"
        a_ [href_ "/admin/categories", class_ "btn btn-ghost"] "Batal"

-- Pengguna --------------------------------------------------------------------------

data UserForm = UserForm
  { ufUsername :: Text
  , ufEmail :: Text
  , ufDisplay :: Text
  , ufRole :: Role
  , ufActive :: Bool
  }

emptyUserForm :: UserForm
emptyUserForm = UserForm "" "" "" RUser True

userToForm :: User -> UserForm
userToForm x = UserForm (userName x) (userEmail x) (userDisplay x) (userRole x) (userActive x)

usersPage :: Ctx -> User -> [User] -> [(Int, Int)] -> Html ()
usersPage c u users counts = adminLayout c u "users" "Pengguna" $ do
  div_ [class_ "toolbar"] $ do
    p_ [class_ "muted"] (toHtml (show (length users) <> " pengguna"))
    a_ [href_ "/admin/users/new", class_ "btn"] "+ Tambah pengguna"
  div_ [class_ "card table-wrap"] $ table_ [class_ "table"] $ do
    thead_ $ tr_ $ do
      th_ "Pengguna"
      th_ "Email"
      th_ "Peran"
      th_ "Status"
      th_ "Artikel"
      th_ "Bergabung"
      th_ [class_ "right"] "Aksi"
    tbody_ $ forM_ users $ \x -> tr_ $ do
      td_ $ div_ [class_ "user-cell"] $ do
        avatar x
        div_ $ do
          strong_ (toHtml (userShownName x))
          br_ []
          small_ [class_ "muted mono"] (toHtml ("@" <> userName x))
      td_ (toHtml (userEmail x))
      td_ (roleBadge (userRole x))
      td_ $
        if userActive x
          then span_ [class_ "badge badge-published"] "Aktif"
          else span_ [class_ "badge badge-draft"] "Nonaktif"
      td_ (toHtml (show (fromMaybe 0 (lookup (userId x) counts))))
      td_ (toHtml (fmtDate (userCreated x)))
      td_ [class_ "right actions"] $ do
        a_ [href_ ("/admin/users/" <> tshow (userId x) <> "/edit"), class_ "btn btn-ghost btn-sm"] "Edit"
        when (userId x /= userId u) $
          deleteButton c ("/admin/users/" <> tshow (userId x) <> "/delete") "Hapus pengguna ini? Artikelnya akan dipindahkan ke akun Anda."

userFormPage :: Ctx -> User -> Maybe User -> UserForm -> [Text] -> Html ()
userFormPage c u mUser UserForm {..} errs =
  adminLayout c u "users" (if isJust mUser then "Edit Pengguna" else "Tambah Pengguna") $
    section_ [class_ "card narrow"] $ do
      errorList errs
      form_ [method_ "post", action_ action, class_ "stack"] $ do
        csrfField c
        inputField "text" "Username" "username" ufUsername [required_ "required", pattern_ "[A-Za-z0-9_]{3,30}"]
        inputField "email" "Email" "email" ufEmail [required_ "required"]
        inputField "text" "Nama tampilan" "display_name" ufDisplay []
        selectField "Peran" "role" [(roleText r, roleLabel r) | r <- [minBound .. maxBound]] (roleText ufRole)
        hint "Admin: akses penuh · Editor: kelola semua artikel & kategori · User: tulis artikel sendiri."
        checkboxField "Akun aktif (boleh login)" "active" ufActive
        inputField "password" (if isJust mUser then "Password baru (kosongkan bila tidak diubah)" else "Password") "password" ""
          ([minLen 8, ac "new-password"] <> [required_ "required" | not (isJust mUser)])
        div_ [class_ "row"] $ do
          button_ [type_ "submit", class_ "btn"] "Simpan"
          a_ [href_ "/admin/users", class_ "btn btn-ghost"] "Batal"
  where
    action = maybe "/admin/users" (\x -> "/admin/users/" <> tshow (userId x)) mUser

-- Profil -----------------------------------------------------------------------------

profilePage :: Ctx -> User -> [Text] -> [Text] -> Html ()
profilePage c u errs pwErrs = adminLayout c u "profile" "Profil" $ do
  section_ [class_ "card avatar-card"] $ do
    avatarSized "avatar-xl" u
    div_ [class_ "avatar-body"] $ do
      h3_ "Foto profil"
      p_ [class_ "muted"] "Pilih foto dan langsung tersimpan. JPG, PNG, WebP, atau GIF, maksimal 2 MB."
      form_ [method_ "post", action_ "/admin/profile/avatar", enctype_ "multipart/form-data", class_ "row avatar-form"] $ do
        csrfField c
        label_ [class_ "btn btn-ghost file-btn"] $ do
          icon "fa-solid fa-image"
          span_ [data_ "file-label" ""] "Ganti foto…"
          input_ [type_ "file", name_ "avatar", accept_ "image/png,image/jpeg,image/webp,image/gif", required_ "required", data_ "avatar-input" ""]
        button_ [type_ "submit", class_ "btn"] (icon "fa-solid fa-upload" >> "Unggah")
      unless (T.null (userAvatar u)) $
        form_ [method_ "post", action_ "/admin/profile/avatar/delete", class_ "inline", data_ "confirm" "Hapus foto profil?"] $ do
          csrfField c
          button_ [type_ "submit", class_ "btn btn-ghost btn-sm"] (icon "fa-regular fa-trash-can" >> "Hapus foto")
  div_ [class_ "grid-2"] $ do
    section_ [class_ "card"] $ do
      h3_ "Data profil"
      errorList errs
      form_ [method_ "post", action_ "/admin/profile", class_ "stack"] $ do
        csrfField c
        field "Username" "u" $ input_ [type_ "text", id_ "u", value_ (userName u), disabled_ "disabled"]
        inputField "email" "Email" "email" (userEmail u) [required_ "required"]
        inputField "text" "Nama tampilan" "display_name" (userDisplay u) []
        textareaField "Bio" "bio" (userBio u) 4 []
        button_ [type_ "submit", class_ "btn"] "Simpan profil"
    section_ [class_ "card"] $ do
      h3_ "Ganti password"
      errorList pwErrs
      form_ [method_ "post", action_ "/admin/profile/password", class_ "stack"] $ do
        csrfField c
        inputField "password" "Password saat ini" "current" "" [required_ "required", ac "current-password"]
        inputField "password" "Password baru" "password" "" [required_ "required", minLen 8, ac "new-password"]
        inputField "password" "Ulangi password baru" "password2" "" [required_ "required", minLen 8, ac "new-password"]
        button_ [type_ "submit", class_ "btn"] "Ganti password"
        hint "Semua sesi lain akan dikeluarkan."

-- Pengaturan ----------------------------------------------------------------------------

settingsPage :: Ctx -> User -> Settings -> [Text] -> Html ()
settingsPage c u Settings {..} errs = adminLayout c u "settings" "Pengaturan Situs" $ do
  -- Logo diunggah lewat form sendiri (multipart), langsung tersimpan begitu dipilih.
  section_ [class_ "card narrow avatar-card logo-card"] $ do
    span_ [class_ "avatar logo-preview"] $
      if T.null setLogo then icon "fa-regular fa-image" else img_ [src_ setLogo, alt_ ""]
    div_ [class_ "avatar-body"] $ do
      h3_ "Logo situs"
      p_ [class_ "muted"] "Tampil di depan nama situs (header, footer, dashboard, login) dan sebagai ikon tab browser. Gunakan gambar persegi, PNG/WebP transparan, maks. 2 MB."
      div_ [class_ "logo-sample"] $ do
        small_ [class_ "muted"] "Contoh tampilan:"
        brandLink c "#" "brand logo-sample-brand"
      form_ [method_ "post", action_ "/admin/settings/logo", enctype_ "multipart/form-data", class_ "row avatar-form"] $ do
        csrfField c
        label_ [class_ "btn btn-ghost file-btn"] $ do
          icon "fa-solid fa-image"
          span_ [data_ "file-label" ""] (if T.null setLogo then "Pilih logo…" else "Ganti logo…")
          input_ [type_ "file", name_ "logo", accept_ "image/png,image/jpeg,image/webp,image/gif", required_ "required", data_ "avatar-input" ""]
        button_ [type_ "submit", class_ "btn"] (icon "fa-solid fa-upload" >> "Unggah")
      unless (T.null setLogo) $
        form_ [method_ "post", action_ "/admin/settings/logo/delete", class_ "inline", data_ "confirm" "Hapus logo situs?"] $ do
          csrfField c
          button_ [type_ "submit", class_ "btn btn-ghost btn-sm"] (icon "fa-regular fa-trash-can" >> "Hapus logo")
  section_ [class_ "card narrow"] $ do
    errorList errs
    form_ [method_ "post", action_ "/admin/settings", class_ "stack"] $ do
      csrfField c
      inputField "text" "Nama situs" "site_name" setSiteName [required_ "required"]
      inputField "text" "Tagline" "tagline" setTagline []
      inputField "number" "Artikel per halaman" "per_page" (tshow setPerPage) [min_ "1", max_ "100"]
      checkboxField "Izinkan pendaftaran pengguna baru" "allow_register" setAllowRegister
      h3_ [class_ "settings-sub"] (icon "fa-solid fa-magnifying-glass" >> " SEO & pratinjau tautan")
      textareaField "Deskripsi situs" "description" setDescription 3 [makeAttribute "maxlength" "300"]
      hint "Muncul di hasil Google dan saat tautan dibagikan di WhatsApp/media sosial. Ideal 120–160 karakter."
      h3_ [class_ "settings-sub"] (icon "fa-brands fa-whatsapp" >> " Tombol WhatsApp")
      inputField "tel" "Nomor WhatsApp" "whatsapp" setWhatsapp [placeholder_ "0812 3456 7890", makeAttribute "inputmode" "tel"]
      hint "Kosongkan untuk menyembunyikan tombol. Nomor diawali 0 otomatis diubah ke 62."
      inputField "text" "Pesan awal" "wa_message" setWaMessage [placeholder_ "Halo, saya ingin bertanya…", makeAttribute "maxlength" "300"]
      h3_ [class_ "settings-sub"] (icon "fa-solid fa-share-nodes" >> " Media sosial")
      hint "Tampil sebagai ikon di footer. Kosongkan yang tidak dipakai."
      forM_ socialKinds $ \(k, lbl, _) ->
        inputField "url" lbl ("social_" <> k) (fromMaybe "" (lookup k setSocial)) [placeholder_ "https://…"]
      button_ [type_ "submit", class_ "btn"] "Simpan pengaturan"

-- Halaman situs ---------------------------------------------------------------------

pagesPage :: Ctx -> User -> [Page] -> Html ()
pagesPage c u pages = adminLayout c u "pages" "Halaman" $ do
  div_ [class_ "card pages-intro"] $ do
    h2_ "Halaman situs"
    p_ [class_ "muted"] $ do
      "Pilih "
      strong_ "Edit di halaman"
      " untuk mengubah teks langsung di halaman, menyeret blok untuk menyusun ulang, dan menambah blok baru dari panel kiri."
  div_ [class_ "card table-wrap"] $ table_ [class_ "table"] $ do
    thead_ $ tr_ $ do
      th_ "Halaman"
      th_ "Alamat"
      th_ "Blok"
      th_ "Diperbarui"
      th_ [class_ "right"] "Aksi"
    tbody_ $ forM_ pages $ \p -> tr_ $ do
      td_ (strong_ (toHtml (pageTitle p)))
      td_ [class_ "mono"] (toHtml (pagePath (pageSlug p)))
      td_ (toHtml (show (length (pageBlocks p))))
      td_ (toHtml (fmtDate (pageUpdated p)))
      td_ [class_ "right actions"] $ do
        a_ [href_ (pagePath (pageSlug p)), target_ "_blank", rel_ "noopener", class_ "btn btn-ghost btn-sm"] (icon "fa-solid fa-eye" >> "Lihat")
        a_ [href_ (pagePath (pageSlug p) <> "?edit=1"), target_ "_blank", rel_ "noopener", class_ "btn btn-sm"] (icon "fa-solid fa-pen" >> "Edit di halaman")

-- SEO artikel ---------------------------------------------------------------------------

-- | Isian SEO + pratinjau hasil Google. Diperbarui langsung oleh static/app.js.
seoPanel :: Ctx -> PostInput -> Html ()
seoPanel c PostInput {..} =
  section_ [class_ "card seo-panel", id_ "seo-panel", data_ "origin" (siteOrigin c), data_ "site" (siteName c)] $ do
    div_ [class_ "seo-head"] $ do
      h3_ (icon "fa-solid fa-magnifying-glass-chart" >> " SEO artikel")
      span_ [class_ "muted"] "Cara artikel tampil di Google & saat dibagikan"
    div_ [class_ "serp", makeAttribute "aria-label" "Pratinjau hasil Google"] $ do
      div_ [class_ "serp-site"] $ do
        span_ [class_ "serp-fav"] (toHtml (T.take 1 (siteName c)))
        div_ $ do
          span_ [class_ "serp-name"] (toHtml (siteName c))
          span_ [class_ "serp-url", id_ "serp-url"] (toHtml (siteOrigin c <> " › post › " <> (if T.null piSlug then "…" else piSlug)))
      div_ [class_ "serp-title", id_ "serp-title"] (toHtml (effectiveTitle (if T.null piTitle then "Judul artikel" else piTitle) piSeo))
      div_ [class_ "serp-desc", id_ "serp-desc"] (toHtml (effectiveDesc piExcerpt piBody piSeo))
    div_ [class_ "seo-grid"] $ do
      inputField "text" "Kata kunci utama" "seo_keyword" (seoKeyword piSeo) [placeholder_ "mis. jasa pembuatan website", makeAttribute "maxlength" "80"]
      counted "text" "Judul SEO" "seo_title" (seoTitle piSeo) 60 "Kosongkan untuk memakai judul artikel."
    countedArea "Deskripsi meta" "seo_desc" (seoDesc piSeo) 160 "Ringkasan 120–160 karakter yang muncul di bawah judul di Google. Ajak orang untuk mengklik."
    inputField "text" "Gambar pratinjau (URL)" "seo_image" (seoImage piSeo) [placeholder_ "https://… atau /asset/img/…"]
    hint "Muncul saat tautan dibagikan di WhatsApp/media sosial. Kosongkan untuk memakai gambar pertama di artikel."
    checkboxField "Sembunyikan artikel ini dari mesin pencari (noindex)" "seo_noindex" (seoNoindex piSeo)
  where
    counted ty label name val mx help =
      field label name $ do
        input_ [type_ ty, id_ name, name_ name, value_ val, makeAttribute "maxlength" "120", data_ "max" (tshow (mx :: Int))]
        div_ [class_ "seo-count"] $ do
          small_ [class_ "muted"] (toHtml (help :: Text))
          span_ [data_ "count-for" name] (toHtml (tshow (T.length val) <> "/" <> tshow mx))
    countedArea label name val mx help =
      field label name $ do
        textarea_ [id_ name, name_ name, rows_ "3", makeAttribute "maxlength" "320", data_ "max" (tshow (mx :: Int))] (toHtml val)
        div_ [class_ "seo-count"] $ do
          small_ [class_ "muted"] (toHtml (help :: Text))
          span_ [data_ "count-for" name] (toHtml (tshow (T.length val) <> "/" <> tshow mx))

-- | Skor SEO + daftar periksa (dirender server, diperbarui langsung oleh JS).
seoScoreCard :: [Check] -> Html ()
seoScoreCard checks =
  div_ [class_ "card seo-score", id_ "seo-score"] $ do
    let n = seoScore checks
        lv = scoreLevel n
    div_ [class_ "seo-score-head"] $ do
      scoreRing "seo-ring" n
      div_ $ do
        h3_ "Skor SEO"
        span_ [class_ ("seo-level " <> levelClass lv), data_ "seo-level" ""] (toHtml (levelLabel lv))
    ul_ [class_ "seo-checks", data_ "seo-checks" ""] $ mapM_ checkItem checks

