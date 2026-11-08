-- | Layout halaman dan komponen bersama.
module CMS.Views.Layout where

import Control.Applicative ((<|>))
import Control.Monad (forM_, unless, when)
import Data.Maybe (isJust)
import Data.Char (isAlphaNum)
import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.URI (urlEncode)
import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Lucid.Base (makeAttribute)

import CMS.Blocks (SitePage (..), pagePath, sitePages)
import CMS.NavStyle (fontLink, lightboxOn, navBottomOn, navCss, revealOn, toTopOn)
import CMS.Util (isBlank)
import CMS.Types
import CMS.Web (paramT, siteOrigin, urlWith)

siteName :: Ctx -> Text
siteName = setSiteName . cSettings

headCommon :: Text -> Html ()
headCommon title = headWith title mempty

-- | Seperti headCommon, dengan isi tambahan di akhir <head>.
headWith :: Text -> Html () -> Html ()
headWith title extra = head_ $ do
  meta_ [charset_ "utf-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  title_ (toHtml title)
  link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
  link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com", crossorigin_ ""]
  link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css2?family=Nunito:wght@400;500;600;700;800&display=swap"]
  link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.2/css/all.min.css", crossorigin_ "anonymous", makeAttribute "referrerpolicy" "no-referrer"]
  link_ [rel_ "stylesheet", href_ "/static/style.css?v=31"]
  script_ [src_ "/static/app.js?v=20", defer_ "defer"] ("" :: Text)
  extra

csrfField :: Ctx -> Html ()
csrfField c = input_ [type_ "hidden", name_ "_csrf", value_ (cCsrf c)]

logoutButton :: Ctx -> Html ()
logoutButton c =
  form_ [method_ "post", action_ "/logout", class_ "inline"] $ do
    csrfField c
    button_ [type_ "submit", class_ "btn btn-ghost btn-sm"] $ do
      icon "fa-solid fa-right-from-bracket"
      "Keluar"

-- | Foto profil, atau huruf awal nama bila belum ada foto.
avatar :: User -> Html ()
avatar = avatarSized ""

avatarSized :: Text -> User -> Html ()
avatarSized extra u
  | T.null (userAvatar u) = span_ [class_ cls] (toHtml (T.toUpper (T.take 1 (userShownName u))))
  | otherwise = span_ [class_ cls] (img_ [src_ (userAvatar u), alt_ ""])
  where
    cls = T.strip ("avatar " <> extra)

-- | Ikon Font Awesome (dekoratif, disembunyikan dari pembaca layar).
icon :: Text -> Html ()
icon cls = i_ [class_ cls, makeAttribute "aria-hidden" "true"] ""

flashBox :: Ctx -> Html ()
flashBox c = forM_ (cFlash c) $ \f ->
  div_ [class_ ("flash flash-" <> flashKind f), role_ "status"] (toHtml (flashMsg f))

errorList :: [Text] -> Html ()
errorList [] = mempty
errorList es = div_ [class_ "flash flash-error", role_ "alert"] $ ul_ (mapM_ (li_ . toHtml) es)

statusBadge :: PostStatus -> Html ()
statusBadge s = span_ [class_ ("badge badge-" <> statusText s)] (toHtml (statusLabel s))

roleBadge :: Role -> Html ()
roleBadge r = span_ [class_ ("badge role-" <> roleText r)] (toHtml (roleLabel r))

pagination :: Text -> [(Text, Text)] -> Int -> Int -> Html ()
pagination base kvs page pages =
  when (pages > 1) $
    nav_ [class_ "pagination"] $ do
      if page > 1
        then a_ [href_ (link (page - 1)), class_ "btn btn-ghost btn-sm"] "‹ Sebelumnya"
        else span_ [] mempty
      span_ [class_ "muted"] (toHtml ("Halaman " <> show page <> " dari " <> show pages))
      if page < pages
        then a_ [href_ (link (page + 1)), class_ "btn btn-ghost btn-sm"] "Berikutnya ›"
        else span_ [] mempty
  where
    link n = urlWith base (kvs ++ [("page", T.pack (show n))])

emptyState :: Text -> Html ()
emptyState msg = div_ [class_ "empty"] (p_ (toHtml msg))

-- Form helpers ------------------------------------------------------------------

field :: Text -> Text -> Html () -> Html ()
field label name inner = div_ [class_ "field"] $ do
  label_ [for_ name] (toHtml label)
  inner

inputField :: Text -> Text -> Text -> Text -> [Attribute] -> Html ()
inputField ty label name val attrs =
  field label name $
    input_ ([type_ ty, id_ name, name_ name, value_ val] <> attrs)

textareaField :: Text -> Text -> Text -> Int -> [Attribute] -> Html ()
textareaField label name val rows attrs =
  field label name $
    textarea_ ([id_ name, name_ name, rows_ (T.pack (show rows))] <> attrs) (toHtml val)

selectField :: Text -> Text -> [(Text, Text)] -> Text -> Html ()
selectField label name opts cur =
  field label name $
    select_ [id_ name, name_ name] $
      forM_ opts $ \(v, l) ->
        option_ ([value_ v] <> [selected_ "selected" | v == cur]) (toHtml l)

checkboxField :: Text -> Text -> Bool -> Html ()
checkboxField label name on =
  div_ [class_ "field field-check"] $
    label_ $ do
      input_ ([type_ "checkbox", name_ name, value_ "1"] <> [checked_ | on])
      toHtml (" " <> label)

hint :: Text -> Html ()
hint = small_ [class_ "hint"] . toHtml

-- Layout publik -------------------------------------------------------------------

-- | Kerangka situs publik: header dengan menu halaman, isi, dan footer.
-- | Meta halaman untuk mesin pencari & pratinjau tautan.
data PageMeta = PageMeta
  { pmTitle :: Text -- isi <title> lengkap
  , pmDesc :: Maybe Text -- Nothing = deskripsi situs
  , pmImage :: Maybe Text -- Nothing = gambar bawaan (menu SEO)
  , pmType :: Text -- "website" | "article"
  , pmNoindex :: Bool
  , pmExtra :: Html () -- tambahan di <head>, mis. JSON-LD
  }

defaultMeta :: Ctx -> Text -> PageMeta
defaultMeta c title = PageMeta (title <> " · " <> siteName c) Nothing Nothing "website" False mempty

siteLayout :: Ctx -> Text -> Text -> [Attribute] -> Html () -> Html ()
siteLayout c active title = siteLayoutMeta c active (defaultMeta c title)

siteLayoutMeta :: Ctx -> Text -> PageMeta -> [Attribute] -> Html () -> Html ()
siteLayoutMeta c active meta bodyAttrs content = doctypehtml_ $ do
  headWith (pmTitle meta) $ do
    seoMeta c meta (not editing)
    forM_ (fontLink nav) $ \u -> link_ [rel_ "stylesheet", href_ u, id_ "theme-font"]
    -- Selalu dirender (walau kosong) supaya builder bisa memperbaruinya untuk pratinjau.
    style_ [id_ "nav-style"] (navCss nav)
  body_ (bodyAttrs <> [data_ "lightbox" "" | lightboxOn nav] <> [data_ "reveal" "" | revealOn nav]) $ do
    siteHeader c active
    content
    siteFooter c
    floatingButtons c
    -- Saat mengedit selalu dirender agar bisa dipratinjau walau sedang dimatikan.
    when (navBottomOn (setNav (cSettings c)) || editing) (tabBar active)
  where
    nav = setNav (cSettings c)
    editing = paramT c "edit" == "1" && maybe False isStaff (cUser c)

-- | Meta untuk mesin pencari & pratinjau tautan (WhatsApp, Facebook, X), favicon,
-- verifikasi Google/Bing, dan Google Analytics (bila diatur di menu SEO).
seoMeta :: Ctx -> PageMeta -> Bool -> Html ()
seoMeta c meta tracking = do
  let s = cSettings c
      ss = setSeo s
      siteDesc = T.strip (if isBlank (setDescription s) then setTagline s else setDescription s)
      desc = maybe siteDesc T.strip (pmDesc meta)
      primary = M.findWithDefault "#0f766e" "theme_primary" (setNav s)
      origin = siteOrigin c
      url = origin <> "/" <> T.intercalate "/" (cPath c)
      absolute u = if "/" `T.isPrefixOf` u && not ("//" `T.isPrefixOf` u) then origin <> u else u
      image = fmap absolute (mfilter' (not . isBlank) (pmImage meta) <|> mfilter' (not . isBlank) (Just (ssDefaultImage ss)))
  unless (T.null desc) $ do
    meta_ [name_ "description", content_ desc]
    meta_ [makeAttribute "property" "og:description", content_ desc]
  when (pmNoindex meta || not (ssIndexable ss)) $
    meta_ [name_ "robots", content_ "noindex, nofollow"]
  link_ [rel_ "canonical", href_ url]
  meta_ [makeAttribute "property" "og:url", content_ url]
  meta_ [makeAttribute "property" "og:title", content_ (pmTitle meta)]
  meta_ [makeAttribute "property" "og:site_name", content_ (siteName c)]
  meta_ [makeAttribute "property" "og:type", content_ (pmType meta)]
  meta_ [makeAttribute "property" "og:locale", content_ "id_ID"]
  forM_ image $ \i -> meta_ [makeAttribute "property" "og:image", content_ i]
  meta_ [name_ "twitter:card", content_ (if isJust image then "summary_large_image" else "summary")]
  meta_ [name_ "theme-color", content_ primary]
  link_ [rel_ "icon", href_ (if T.null (setLogo s) then faviconUri primary (siteName c) else setLogo s)]
  unless (T.null (ssGoogleVerify ss)) $ meta_ [name_ "google-site-verification", content_ (ssGoogleVerify ss)]
  unless (T.null (ssBingVerify ss)) $ meta_ [name_ "msvalidate.01", content_ (ssBingVerify ss)]
  pmExtra meta
  -- Google Analytics 4 (ID sudah divalidasi saat disimpan: G-XXXX).
  when (tracking && not (T.null (ssGaId ss))) $ do
    script_ [async_ "", src_ ("https://www.googletagmanager.com/gtag/js?id=" <> ssGaId ss)] ("" :: Text)
    script_ ("window.dataLayer=window.dataLayer||[];function gtag(){dataLayer.push(arguments)}gtag('js',new Date());gtag('config','" <> ssGaId ss <> "');" :: Text)
  where
    mfilter' p m = case m of
      Just x | p x -> Just x
      _ -> Nothing

-- | Logo + nama situs ("[logo] DCMS"). Tanpa logo: penanda kotak kecil lewat CSS.
brandLink :: Ctx -> Text -> Text -> Html ()
brandLink c href cls =
  a_ [href_ href, class_ (cls <> if T.null logo then "" else " has-logo")] $ do
    unless (T.null logo) $ img_ [src_ logo, alt_ "", class_ "brand-logo"]
    span_ [class_ "brand-name"] (toHtml (siteName c))
  where
    logo = setLogo (cSettings c)

-- | Favicon SVG: huruf awal nama situs di kotak membulat berwarna utama.
faviconUri :: Text -> Text -> Text
faviconUri color name =
  let letter = case T.uncons (T.toUpper (T.strip name)) of
        Just (ch, _) | isAlphaNum ch -> T.singleton ch
        _ -> "•"
      svg =
        "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 64 64'><rect width='64' height='64' rx='16' fill='"
          <> color
          <> "'/><text x='32' y='44' font-family='system-ui,sans-serif' font-size='36' font-weight='800' fill='#fff' text-anchor='middle'>"
          <> letter
          <> "</text></svg>"
   in "data:image/svg+xml," <> T.concatMap enc svg
  where
    enc ch = case ch of
      '#' -> "%23"
      '<' -> "%3C"
      '>' -> "%3E"
      '"' -> "%22"
      ' ' -> "%20"
      _ -> T.singleton ch

-- | Tombol melayang kanan bawah: WhatsApp (bila nomornya diisi) dan kembali ke atas.
floatingButtons :: Ctx -> Html ()
floatingButtons c = do
  let s = cSettings c
      wa = setWhatsapp s
      waUrl =
        "https://wa.me/" <> wa
          <> if isBlank (setWaMessage s) then "" else "?text=" <> TE.decodeUtf8 (urlEncode True (TE.encodeUtf8 (setWaMessage s)))
      toTop = toTopOn (setNav s)
  when (not (T.null wa) || toTop) $
    div_ [class_ "fab-stack"] $ do
      when toTop $
        button_ [type_ "button", class_ "fab totop", data_ "totop" "", title_ "Kembali ke atas", makeAttribute "aria-label" "Kembali ke atas", makeAttribute "hidden" ""] $
          icon "fa-solid fa-arrow-up"
      unless (T.null wa) $
        a_ [href_ waUrl, target_ "_blank", rel_ "noopener", class_ "fab wa-fab", title_ "Chat WhatsApp", makeAttribute "aria-label" "Chat lewat WhatsApp"] $
          icon "fa-brands fa-whatsapp"

-- | Header situs. Di HP menu halaman disembunyikan di balik tombol ☰ (panel turun,
-- static/app.js [data-nav-toggle]); di layar lebar tampil sebagai kapsul menu.
siteHeader :: Ctx -> Text -> Html ()
siteHeader c active =
  header_ [class_ "site-header", data_ "site-header" ""] $
    div_ [class_ "wrap header-inner"] $ do
      brandLink c "/" "brand"
      nav_ [class_ "site-menu", id_ "site-menu", makeAttribute "aria-label" "Menu utama"] $
        forM_ sitePages $ \sp ->
          a_ ([href_ (pagePath (spSlug sp))] <> [class_ "active" | spSlug sp == active] <> [makeAttribute "aria-current" "page" | spSlug sp == active]) $ do
            icon (pageIcon (spSlug sp))
            span_ (toHtml (spTitle sp))
      div_ [class_ "site-actions"] $ do
        case cUser c of
          Just _ -> a_ [href_ "/admin", class_ "btn btn-sm site-cta", title_ "Dashboard"] (icon "fa-solid fa-gauge-high" >> span_ [class_ "site-cta-label"] "Dashboard")
          Nothing -> a_ [href_ "/login", class_ "btn btn-ghost btn-sm site-cta", title_ "Masuk"] (icon "fa-solid fa-arrow-right-to-bracket" >> span_ [class_ "site-cta-label"] "Masuk")
        button_
          [ type_ "button", class_ "nav-toggle", data_ "nav-toggle" ""
          , makeAttribute "aria-controls" "site-menu", makeAttribute "aria-expanded" "false", makeAttribute "aria-label" "Buka menu"
          ]
          (span_ [class_ "nav-toggle-bars", makeAttribute "aria-hidden" "true"] (span_ "" >> span_ "" >> span_ ""))

-- | Navigasi bawah ala iOS (ikon + judul). Muncul saat halaman digulir ke bawah (app.js).
tabBar :: Text -> Html ()
tabBar active =
  nav_ [class_ "tabbar", makeAttribute "aria-label" "Navigasi cepat", makeAttribute "inert" ""] $
    forM_ sitePages $ \sp ->
      a_ ([href_ (pagePath (spSlug sp))] <> [class_ "active" | spSlug sp == active]) $ do
        icon (pageIcon (spSlug sp))
        span_ (toHtml (spTitle sp))

pageIcon :: Text -> Text
pageIcon = \case
  "home" -> "fa-solid fa-house"
  "about" -> "fa-solid fa-users"
  "services" -> "fa-solid fa-briefcase"
  "branch" -> "fa-solid fa-location-dot"
  "blog" -> "fa-regular fa-newspaper"
  "contact" -> "fa-solid fa-envelope"
  _ -> "fa-regular fa-file"

siteFooter :: Ctx -> Html ()
siteFooter c =
  footer_ [class_ "site-footer"] $
    div_ [class_ "wrap footer-inner"] $ do
      div_ $ do
        brandLink c "/" "brand"
        p_ [class_ "muted"] (toHtml (setTagline (cSettings c)))
      nav_ [class_ "footer-menu"] $
        forM_ sitePages $ \sp -> a_ [href_ (pagePath (spSlug sp))] (toHtml (spTitle sp))
      let social = setSocial (cSettings c)
      unless (null social) $
        div_ [class_ "footer-social"] $
          forM_ socialKinds $ \(k, lbl, ic) -> forM_ (lookup k social) $ \url ->
            a_ [href_ url, target_ "_blank", rel_ "noopener me", title_ lbl, makeAttribute "aria-label" lbl] (icon ic)
      p_ [class_ "muted footer-copy"] (toHtml ("© " <> siteName c))

-- | Halaman artikel & arsip kategori: lebar baca sempit di bawah header situs.
publicLayout :: Ctx -> [Category] -> Text -> Html () -> Html ()
publicLayout c cats title = publicLayoutMeta c cats (defaultMeta c title)

publicLayoutMeta :: Ctx -> [Category] -> PageMeta -> Html () -> Html ()
publicLayoutMeta c cats meta content =
  siteLayoutMeta c "blog" meta [class_ "site"] $ do
    let visible = filter ((> 0) . catCount) cats
    unless (null visible) $
      nav_ [class_ "cat-bar"] $
        div_ [class_ "container"] $ do
          a_ [href_ "/blog"] "Semua"
          forM_ visible $ \k -> a_ [href_ ("/category/" <> catSlug k)] (toHtml (catName k))
    main_ [class_ "container main"] $ do
      flashBox c
      content

-- | Layout kartu di tengah: login, register, halaman pesan.
authLayout :: Ctx -> Text -> Html () -> Html ()
authLayout c title content = doctypehtml_ $ do
  headCommon (title <> " · " <> siteName c)
  body_ [class_ "auth"] $
    main_ [class_ "auth-wrap"] $ do
      brandLink c "/" "brand auth-brand"
      div_ [class_ "card auth-card"] $ do
        flashBox c
        content

messagePage :: Ctx -> Text -> Text -> Html ()
messagePage c title msg = authLayout c title $ do
  h1_ (toHtml title)
  p_ [class_ "muted"] (toHtml msg)
  p_ $ a_ [href_ (if maybe False (const True) (cUser c) then "/admin" else "/"), class_ "btn"] "Kembali"

-- Layout admin ---------------------------------------------------------------------

-- | Item menu sidebar admin. Item dengan anak tampil sebagai grup dengan sub menu.
data NavItem = NavItem
  { navKey :: Text
  , navLabel :: Text
  , navHref :: Text
  , navIcon :: Text
  , navChildren :: [NavItem]
  }

navItems :: User -> [NavItem]
navItems u =
  [leaf "dashboard" "Dashboard" "/admin" "fa-solid fa-gauge-high"]
    ++ [leaf "pages" "Halaman" "/admin/pages" "fa-regular fa-file-lines" | isStaff u]
    ++ [leaf "branches" "Cabang" "/admin/branches" "fa-solid fa-store" | isStaff u]
    ++ [leaf "seo" "SEO & Analitik" "/admin/seo" "fa-solid fa-chart-line" | isStaff u]
    ++ [leaf "api" "API Gateway" "/admin/api" "fa-solid fa-plug" | isAdmin u]
    ++ [ NavItem "articles" "Artikel" "/admin/posts" "fa-regular fa-newspaper" $
           [ leaf "posts" "Semua Artikel" "/admin/posts" "fa-solid fa-list-ul"
           , leaf "new" "Tulis Baru" "/admin/posts/new" "fa-solid fa-pen-to-square"
           ]
             ++ [leaf "categories" "Kategori" "/admin/categories" "fa-solid fa-tags" | isStaff u]
       ]
    ++ [leaf "users" "Pengguna" "/admin/users" "fa-solid fa-users" | isAdmin u]
    ++ [leaf "settings" "Pengaturan" "/admin/settings" "fa-solid fa-gear" | isAdmin u]
    ++ [leaf "profile" "Profil" "/admin/profile" "fa-regular fa-circle-user"]
  where
    leaf k l h i = NavItem k l h i []

-- | Sub menu terbuka bila salah satu anaknya sedang aktif.
sideNav :: Text -> [NavItem] -> Html ()
sideNav active items =
  nav_ [class_ "side-nav"] $
    forM_ items $ \n -> case navChildren n of
      [] -> navLink n
      kids -> do
        let open = any ((== active) . navKey) kids
        div_ [class_ ("side-group" <> if open then " open" else "")] $ do
          a_ [href_ (navHref n), class_ "side-parent"] $ do
            icon (navIcon n)
            span_ (toHtml (navLabel n))
            i_ [class_ "fa-solid fa-chevron-down side-caret", makeAttribute "aria-hidden" "true"] ""
          div_ [class_ "side-sub"] (mapM_ navLink kids)
  where
    navLink :: NavItem -> Html ()
    navLink n =
      a_ ([href_ (navHref n)] <> [class_ "active" | navKey n == active]) $ do
        icon (navIcon n)
        span_ (toHtml (navLabel n))

adminLayout :: Ctx -> User -> Text -> Text -> Html () -> Html ()
adminLayout c u active title content = doctypehtml_ $ do
  headCommon (title <> " · Admin " <> siteName c)
  body_ [class_ "admin"] $
    div_ [class_ "admin-shell"] $ do
      aside_ [class_ "sidebar"] $ do
        brandLink c "/admin" "brand"
        sideNav active (navItems u)
        div_ [class_ "sidebar-foot"] $ do
          div_ [class_ "user-chip side-box"] $ do
            avatar u
            div_ [class_ "user-meta"] $ do
              strong_ (toHtml (userShownName u))
              roleBadge (userRole u)
          a_ [href_ "/", target_ "_blank", rel_ "noopener", class_ "side-box"] $ do
            icon "fa-solid fa-arrow-up-right-from-square"
            span_ "Lihat situs"
          form_ [method_ "post", action_ "/logout", class_ "inline"] $ do
            csrfField c
            button_ [type_ "submit", class_ "side-box"] $ do
              icon "fa-solid fa-right-from-bracket"
              span_ "Keluar"
      div_ [class_ "admin-main"] $ do
        main_ [class_ "content"] $ do
          flashBox c
          content

ac :: Text -> Attribute
ac = makeAttribute "autocomplete"

minLen :: Int -> Attribute
minLen n = makeAttribute "minlength" (T.pack (show n))
