-- | Menu Halaman di dashboard: daftar, buat baru (templat/salin), dan pengaturan halaman.
module CMS.Views.PageAdmin (pagesPage, pageFormPage, PageFilter (..)) where

import Control.Monad (forM_, unless, when)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Lucid.Base (makeAttribute)

import CMS.Blocks
import CMS.Types
import CMS.Util
import CMS.Views.Layout
import CMS.Web (siteOrigin)

-- | Pencarian & filter di daftar halaman.
data PageFilter = PageFilter
  { pfQuery :: Text
  , pfStatus :: Text -- "" | "published" | "draft" | "hidden"
  }

-- Daftar halaman --------------------------------------------------------------------------

pagesPage :: Ctx -> User -> PageFilter -> [Page] -> [Page] -> Html ()
pagesPage c u flt allPages shown = adminLayout c u "pages" "Halaman" $ do
  div_ [class_ "card pages-intro toolbar"] $ do
    div_ $ do
      h2_ "Halaman situs"
      p_ [class_ "muted"] $ do
        "Buat halaman baru dari templat, lalu pilih "
        strong_ "Edit di halaman"
        " untuk mengubah teks langsung, menyeret blok, dan menambah blok dari panel kiri."
    a_ [href_ "/admin/pages/new", class_ "btn"] (icon "fa-solid fa-plus" >> "Halaman baru")
  div_ [class_ "stats pages-stats"] $ do
    stat "Semua halaman" (length allPages) ""
    stat "Terbit" (count pagePublished) "published"
    stat "Draf" (count (not . pagePublished)) "draft"
    stat "Tidak di menu" (count (not . pageInMenu)) "hidden"
  form_ [method_ "get", action_ "/admin/pages", class_ "filters pages-filters"] $ do
    input_ [type_ "search", name_ "q", value_ (pfQuery flt), placeholder_ "Cari judul atau alamat…", makeAttribute "aria-label" "Cari halaman"]
    select_ [name_ "status", makeAttribute "aria-label" "Filter status", onchange_ "this.form.submit()"] $
      forM_ [("", "Semua status"), ("published", "Terbit"), ("draft", "Draf"), ("hidden", "Tidak di menu")] $ \(v, l) ->
        option_ ([value_ v] <> [selected_ "selected" | v == pfStatus flt]) (toHtml (l :: Text))
    button_ [type_ "submit", class_ "btn btn-ghost btn-sm"] (icon "fa-solid fa-magnifying-glass" >> "Cari")
    when filtering $ a_ [href_ "/admin/pages", class_ "btn btn-ghost btn-sm"] "Reset"
  div_ [class_ "card table-wrap pages-wrap"] $
    if null shown
      then emptyState (if filtering then "Tidak ada halaman yang cocok." else "Belum ada halaman.")
      else table_ [class_ "table pages-table"] $ do
        thead_ $ tr_ $ do
          th_ [class_ "pages-order-col"] (span_ [class_ "sr-only"] "Urutan")
          th_ "Halaman"
          th_ "Alamat"
          th_ "Blok"
          th_ "Diperbarui"
          th_ [class_ "right"] "Aksi"
        tbody_ $ forM_ shown $ \p -> tr_ $ do
          let slug = pageSlug p
              base = "/admin/pages/" <> slug
              idx = length (takeWhile ((/= slug) . pageSlug) allPages)
          td_ [class_ "pages-order"] $ do
            moveBtn base "up" "fa-solid fa-chevron-up" "Naikkan di menu" (idx == 0)
            moveBtn base "down" "fa-solid fa-chevron-down" "Turunkan di menu" (idx >= length allPages - 1)
          td_ $ do
            div_ [class_ "page-name"] $ do
              icon (if isBlank (pageMenuIcon p) then defaultPageIcon slug else pageMenuIcon p)
              strong_ (toHtml (pageTitle p))
            div_ [class_ "page-badges"] $ do
              if pagePublished p
                then span_ [class_ "badge badge-published"] "Terbit"
                else span_ [class_ "badge badge-draft"] "Draf"
              unless (pageInMenu p) $ span_ [class_ "badge badge-pending"] "Tidak di menu"
              when (seoNoindex (pageSeo p)) $ span_ [class_ "badge badge-draft"] "noindex"
              when (isSystemPage slug) $ span_ [class_ "badge role-editor", title_ "Halaman bawaan: tidak bisa dihapus"] "Bawaan"
          td_ [class_ "mono"] (toHtml (pagePath slug))
          td_ (toHtml (show (length (pageBlocks p))))
          td_ (toHtml (fmtDate (pageUpdated p)))
          td_ [class_ "right"] $ div_ [class_ "page-actions"] $ do
            a_ [href_ (pagePath slug), target_ "_blank", rel_ "noopener", class_ "btn btn-ghost btn-sm btn-icon", title_ "Lihat halaman", makeAttribute "aria-label" "Lihat halaman"] (icon "fa-solid fa-eye")
            a_ [href_ (pagePath slug <> "?edit=1"), target_ "_blank", rel_ "noopener", class_ "btn btn-sm", title_ "Edit isi langsung di halaman"] (icon "fa-solid fa-pen" >> "Edit di halaman")
            a_ [href_ (base <> "/settings"), class_ "btn btn-ghost btn-sm", title_ "Judul, alamat, menu, dan SEO"] (icon "fa-solid fa-sliders" >> span_ [class_ "lbl"] "Pengaturan")
            details_ [class_ "row-menu"] $ do
              summary_ [class_ "btn btn-ghost btn-sm btn-icon", title_ "Aksi lain", makeAttribute "aria-label" "Aksi lain"] (icon "fa-solid fa-ellipsis")
              div_ [class_ "row-menu-panel"] $ do
                unless (slug == "home") $
                  postBtn base "toggle" (if pagePublished p then "fa-solid fa-eye-slash" else "fa-solid fa-upload") (if pagePublished p then "Jadikan draf" else "Terbitkan") Nothing
                postBtn base "duplicate" "fa-regular fa-copy" "Duplikat" Nothing
                unless (isSystemPage slug) $
                  postBtn base "delete" "fa-regular fa-trash-can" "Hapus" (Just ("Hapus halaman “" <> pageTitle p <> "” beserta isinya? Tindakan ini tidak bisa dibatalkan."))
  p_ [class_ "muted hint pages-note"] $ do
    icon "fa-solid fa-circle-info"
    " Urutan di tabel = urutan di menu atas & footer. Menu bawah (HP) menampilkan 6 halaman pertama."
  where
    filtering = not (isBlank (pfQuery flt)) || not (T.null (pfStatus flt))
    count f = length (filter f allPages)
    stat :: Text -> Int -> Text -> Html ()
    stat label n st =
      a_ [href_ (if T.null st then "/admin/pages" else "/admin/pages?status=" <> st), class_ ("card stat" <> if pfStatus flt == st then " active" else "")] $ do
        span_ [class_ "stat-label"] (toHtml label)
        span_ [class_ "stat-value"] (toHtml (show n))
    moveBtn :: Text -> Text -> Text -> Text -> Bool -> Html ()
    moveBtn base dir ic tip disabled =
      form_ [method_ "post", action_ (base <> "/move"), class_ "inline"] $ do
        csrfField c
        input_ [type_ "hidden", name_ "dir", value_ dir]
        button_ ([type_ "submit", class_ "btn btn-ghost btn-sm btn-icon", title_ tip, makeAttribute "aria-label" tip] <> [disabled_ "disabled" | disabled]) (icon ic)
    postBtn :: Text -> Text -> Text -> Text -> Maybe Text -> Html ()
    postBtn base act ic label confirm =
      form_ ([method_ "post", action_ (base <> "/" <> act), class_ "inline"] <> maybe [] (\m -> [data_ "confirm" m]) confirm) $ do
        csrfField c
        button_ [type_ "submit", class_ ("row-menu-item" <> if act == "delete" then " danger" else "")] (icon ic >> toHtml label)

-- Form buat & pengaturan ------------------------------------------------------------------

-- | @mPage@ = Nothing untuk halaman baru. @others@ = halaman yang bisa disalin isinya.
pageFormPage :: Ctx -> User -> Maybe Page -> [Page] -> PageInput -> [Text] -> Html ()
pageFormPage c u mPage others PageInput {..} errs =
  adminLayout c u "pages" (if isNew then "Halaman baru" else "Pengaturan halaman") $ do
    div_ [class_ "page-form-head"] $ do
      a_ [href_ "/admin/pages", class_ "btn btn-ghost btn-sm"] (icon "fa-solid fa-arrow-left" >> "Semua halaman")
      h2_ (toHtml (if isNew then "Buat halaman baru" else "Pengaturan: " <> maybe "" pageTitle mPage))
    errorList errs
    form_ [method_ "post", action_ action, class_ "editor-layout", id_ "page-form", data_ "origin" (siteOrigin c), data_ "site" (siteName c)] $ do
      csrfField c
      div_ [class_ "editor-main"] $ do
        input_
          [ type_ "text", name_ "title", id_ "page-title", value_ pgiTitle, class_ "title-input"
          , placeholder_ "Judul halaman, mis. Promo Akhir Tahun", required_ "required", makeAttribute "maxlength" "80"
          , makeAttribute "aria-label" "Judul halaman", autofocus_
          ]
        div_ [class_ "slug-row"] $ do
          span_ [class_ "muted mono"] (toHtml (siteOrigin c <> "/"))
          if lockedSlug
            then do
              input_ [type_ "text", id_ "page-slug", value_ pgiSlug, class_ "mono", disabled_ "disabled"]
              input_ [type_ "hidden", name_ "slug", value_ pgiSlug]
            else input_ [type_ "text", name_ "slug", id_ "page-slug", value_ pgiSlug, class_ "mono", placeholder_ "otomatis-dari-judul", makeAttribute "maxlength" "60", pattern_ "[a-z0-9]+(-[a-z0-9]+)*", title_ "Huruf kecil, angka, dan tanda hubung"]
        if lockedSlug
          then hint "Alamat halaman bawaan tidak bisa diubah."
          else hint (if isNew then "Kosongkan untuk dibuat otomatis dari judul. Hanya huruf kecil, angka, dan tanda hubung." else "Mengubah alamat membuat tautan lama tidak berlaku lagi.")
        when isNew $ section_ [class_ "card"] $ do
          h3_ (icon "fa-solid fa-layer-group" >> " Mulai dari templat")
          p_ [class_ "muted"] "Semua blok bisa diubah, dipindah, atau dihapus nanti di editor halaman."
          div_ [class_ "tpl-grid", role_ "radiogroup", makeAttribute "aria-label" "Templat halaman"] $
            forM_ pageTemplates $ \t ->
              label_ [class_ "tpl-card"] $ do
                input_ ([type_ "radio", name_ "template", value_ (ptKey t)] <> [checked_ | ptKey t == pgiTemplate])
                span_ [class_ "tpl-ico"] (icon (ptIcon t))
                span_ [class_ "tpl-body"] $ do
                  strong_ (toHtml (ptLabel t))
                  small_ [class_ "muted"] (toHtml (ptDesc t))
                  small_ [class_ "tpl-blocks"] (toHtml (T.intercalate " · " (blockNames (ptBlocks t))))
          unless (null others) $ do
            selectField
              "Atau salin isi dari halaman yang sudah ada"
              "copy_from"
              (("", "— Tidak, pakai templat di atas —") : [(pageSlug p, pageTitle p <> "  (" <> pagePath (pageSlug p) <> ")") | p <- others])
              pgiCopyFrom
            hint "Semua blok halaman itu disalin ke halaman baru. Templat di atas diabaikan."
        section_ [class_ "card seo-panel"] $ do
          div_ [class_ "seo-head"] $ do
            h3_ (icon "fa-solid fa-magnifying-glass-chart" >> " SEO halaman")
            span_ [class_ "muted"] "Cara halaman tampil di Google & saat dibagikan"
          div_ [class_ "serp", makeAttribute "aria-label" "Pratinjau hasil Google"] $ do
            div_ [class_ "serp-site"] $ do
              span_ [class_ "serp-fav"] (toHtml (T.take 1 (siteName c)))
              div_ $ do
                span_ [class_ "serp-name"] (toHtml (siteName c))
                span_ [class_ "serp-url", id_ "pserp-url"] (toHtml (siteOrigin c <> " › " <> pgiSlug))
            div_ [class_ "serp-title", id_ "pserp-title"] (toHtml (if isBlank (seoTitle pgiSeo) then (if T.null pgiTitle then "Judul halaman" else pgiTitle) <> " · " <> siteName c else seoTitle pgiSeo))
            div_ [class_ "serp-desc", id_ "pserp-desc", data_ "default" siteDesc] (toHtml (if isBlank (seoDesc pgiSeo) then siteDesc else seoDesc pgiSeo))
          counted "Judul SEO" "seo_title" (seoTitle pgiSeo) 60 "Kosongkan untuk memakai “Judul halaman · Nama situs”."
          countedArea "Deskripsi meta" "seo_desc" (seoDesc pgiSeo) 160 "Ringkasan 120–160 karakter di bawah judul di Google. Kosongkan untuk memakai deskripsi situs."
          inputField "text" "Gambar pratinjau (URL)" "seo_image" (seoImage pgiSeo) [placeholder_ "https://… atau /asset/img/…", makeAttribute "maxlength" "1000"]
          hint "Muncul saat tautan dibagikan di WhatsApp/media sosial. Kosongkan untuk memakai gambar bawaan dari menu SEO."
          checkboxField "Sembunyikan halaman ini dari mesin pencari (noindex)" "seo_noindex" (seoNoindex pgiSeo)
      aside_ [class_ "editor-side"] $ do
        div_ [class_ "card"] $ do
          h3_ "Publikasi"
          if isHome
            then do
              input_ [type_ "hidden", name_ "status", value_ "published"]
              p_ [class_ "muted"] "Beranda selalu terbit."
            else selectField "Status" "status" [("published", "Terbit"), ("draft", "Draf (hanya terlihat Admin & Editor)")] (if pgiPublished then "published" else "draft")
          label_ [class_ "switch-row"] $ do
            span_ "Tampilkan di menu"
            input_ ([type_ "checkbox", name_ "in_menu", value_ "1"] <> [checked_ | pgiInMenu])
          hint "Menu atas, menu bawah (HP), dan footer."
          button_ [type_ "submit", class_ "btn btn-block"] (if isNew then icon "fa-solid fa-wand-magic-sparkles" >> "Buat & mulai edit" else icon "fa-solid fa-floppy-disk" >> "Simpan pengaturan")
        div_ [class_ "card"] $ do
          h3_ "Ikon menu"
          div_ [class_ "icon-pick-row"] $ do
            span_ [class_ "icon-preview", id_ "icon-preview"] (icon (if isBlank pgiIcon then "fa-regular fa-file-lines" else pgiIcon))
            input_ [type_ "text", name_ "icon", id_ "page-icon", value_ pgiIcon, class_ "mono", placeholder_ "fa-regular fa-file-lines", makeAttribute "maxlength" "80", makeAttribute "aria-label" "Kelas ikon Font Awesome"]
          div_ [class_ "icon-picks"] $
            forM_ iconChoices $ \ic ->
              button_ [type_ "button", class_ ("icon-pick" <> if ic == pgiIcon then " active" else ""), data_ "icon" ic, title_ ic, makeAttribute "aria-label" ic] (icon ic)
          hint "Pilih salah satu, atau tulis kelas ikon Font Awesome sendiri."
        forM_ mPage $ \p -> div_ [class_ "card meta-card"] $ do
          h3_ "Info"
          dl_ $ do
            dt_ "Blok" >> dd_ (toHtml (show (length (pageBlocks p))))
            unless (T.null (pageCreated p)) $ dt_ "Dibuat" >> dd_ (toHtml (fmtDate (pageCreated p)))
            dt_ "Diperbarui" >> dd_ (toHtml (fmtDate (pageUpdated p)))
          a_ [href_ (pagePath (pageSlug p) <> "?edit=1"), target_ "_blank", rel_ "noopener", class_ "btn btn-block"] (icon "fa-solid fa-pen" >> "Edit isi di halaman")
          a_ [href_ (pagePath (pageSlug p)), target_ "_blank", rel_ "noopener", class_ "btn btn-ghost btn-block"] "Lihat di situs ↗"
          unless (isSystemPage (pageSlug p)) $
            button_ [type_ "submit", form_ "delete-form", class_ "btn btn-danger btn-block"] "Hapus halaman"
    forM_ mPage $ \p -> unless (isSystemPage (pageSlug p)) $
      form_ [method_ "post", action_ ("/admin/pages/" <> pageSlug p <> "/delete"), id_ "delete-form", data_ "confirm" ("Hapus halaman “" <> pageTitle p <> "” beserta isinya?")] $
        csrfField c
  where
    isNew = not (isJust mPage)
    isHome = maybe False ((== "home") . pageSlug) mPage
    lockedSlug = maybe False (isSystemPage . pageSlug) mPage
    action = maybe "/admin/pages" (\p -> "/admin/pages/" <> pageSlug p <> "/settings") mPage
    s = cSettings c
    siteDesc = if isBlank (setDescription s) then setTagline s else setDescription s
    blockNames bs = [maybe (bType b) spLabel (lookupSpec (bType b)) | b <- bs]
    counted :: Text -> Text -> Text -> Int -> Text -> Html ()
    counted label name val mx help =
      field label name $ do
        input_ [type_ "text", id_ name, name_ name, value_ val, makeAttribute "maxlength" "120", data_ "max" (tshow mx)]
        div_ [class_ "seo-count"] $ do
          small_ [class_ "muted"] (toHtml help)
          span_ [data_ "count-for" name] (toHtml (tshow (T.length val) <> "/" <> tshow mx))
    countedArea :: Text -> Text -> Text -> Int -> Text -> Html ()
    countedArea label name val mx help =
      field label name $ do
        textarea_ [id_ name, name_ name, rows_ "3", makeAttribute "maxlength" "320", data_ "max" (tshow mx)] (toHtml val)
        div_ [class_ "seo-count"] $ do
          small_ [class_ "muted"] (toHtml help)
          span_ [data_ "count-for" name] (toHtml (tshow (T.length val) <> "/" <> tshow mx))

-- | Pilihan cepat ikon menu.
iconChoices :: [Text]
iconChoices =
  [ "fa-regular fa-file-lines", "fa-solid fa-house", "fa-solid fa-users", "fa-solid fa-briefcase", "fa-solid fa-location-dot"
  , "fa-solid fa-envelope", "fa-solid fa-tags", "fa-solid fa-circle-question", "fa-regular fa-images", "fa-solid fa-rocket"
  , "fa-solid fa-gift", "fa-solid fa-cart-shopping", "fa-solid fa-calendar-days", "fa-solid fa-star", "fa-solid fa-heart"
  , "fa-solid fa-graduation-cap", "fa-solid fa-handshake", "fa-solid fa-newspaper", "fa-solid fa-phone", "fa-solid fa-circle-info"
  ]
