-- | Halaman situs dari page builder: render blok + mode edit di tempat.
--
-- Satu renderer dipakai untuk tampilan biasa dan mode edit. Di mode edit, setiap
-- isian diberi atribut data-f (teks) / data-fa (atribut seperti URL atau ikon)
-- yang dibaca kembali oleh static/builder.js saat menyimpan.
module CMS.Views.Site (pageView, blogListing) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Lucid.Base (makeAttribute)

import CMS.Blocks
import CMS.NavStyle
import CMS.Types
import CMS.Util
import CMS.Views.Layout

-- Halaman ------------------------------------------------------------------------------

-- | Halaman builder. @extra@ dirender di bawah blok (dipakai daftar artikel di Blog).
pageView :: Ctx -> Page -> Bool -> [Post] -> Html () -> Html ()
pageView c pg edit posts extra =
  siteLayout c (pageSlug pg) (pageTitle pg) bodyAttrs $ do
    when edit (editorBar c pg)
    main_ [class_ "site-main"] $ do
      forM_ (cFlash c) $ \_ -> div_ [class_ "wrap flash-wrap"] (flashBox c)
      div_ [id_ "pb-canvas"] (mapM_ (renderBlock edit posts) (pageBlocks pg))
      extra
    if edit
      then editorPalette (setNav (cSettings c)) posts
      else when canEdit $
        a_ [href_ (pagePath (pageSlug pg) <> "?edit=1"), target_ "_blank", rel_ "noopener", class_ "pb-fab"] $ do
          icon "fa-solid fa-pen"
          "Edit halaman"
  where
    canEdit = maybe False isStaff (cUser c)
    bodyAttrs = if edit then [class_ "site pb-editing", data_ "page" (pageSlug pg)] else [class_ "site"]

-- | Daftar artikel di halaman Blog (tidak bisa diedit: isinya dari menu Artikel).
blogListing :: Bool -> [Category] -> [Post] -> Text -> Int -> Int -> Html ()
blogListing edit cats posts q page pages =
  section_ [class_ ("pb blog-list" <> if edit then " pb-locked" else "")] $
    div_ [class_ "wrap"] $ do
      when edit $
        p_ [class_ "pb-locked-note"] $ do
          icon "fa-solid fa-lock"
          " Daftar artikel diisi otomatis dari menu Artikel."
      div_ [class_ "blog-tools"] $ do
        form_ [method_ "get", action_ "/blog", class_ "search", role_ "search"] $
          input_ [type_ "search", name_ "q", placeholder_ "Cari artikel…", value_ q, makeAttribute "aria-label" "Cari artikel"]
        let visible = filter ((> 0) . catCount) cats
        unless (null visible) $
          nav_ [class_ "chips"] $ do
            a_ [href_ "/blog", class_ "chip"] "Semua"
            forM_ visible $ \k -> a_ [href_ ("/category/" <> catSlug k), class_ "chip"] (toHtml (catName k))
      unless (isBlank q) $
        p_ [class_ "muted"] (toHtml ("Hasil pencarian untuk “" <> q <> "”"))
      if null posts
        then emptyState "Belum ada artikel yang cocok."
        else div_ [class_ "pb-grid pb-posts-grid"] (mapM_ postTile posts)
      pagination "/blog" [("q", q)] page pages

-- Blok ---------------------------------------------------------------------------------

renderBlock :: Bool -> [Post] -> Block -> Html ()
renderBlock edit posts b = case bType b of
  "hero" -> sectionWith heroCls $ do
    when edit $
      span_
        [ class_ "pb-chip hero-chip", data_ "hero-chip" ""
        , data_ "fa" "src:data-src,alt:data-alt,layout:data-layout"
        , data_ "src" heroSrc, data_ "alt" (fieldOf "alt" fs), data_ "layout" heroLayout
        ]
        (icon "fa-regular fa-image" >> " Gambar hero")
    div_ [class_ "hero-grid"] $ do
      div_ [class_ "hero-inner"] $ do
        txt fs p_ [class_ "eyebrow"] "eyebrow" "Label kecil"
        txt fs h1_ [] "title" "Judul besar"
        txt fs p_ [class_ "lead ml"] "text" "Kalimat pembuka"
        actions $ do
          btnLink fs "btn btn-lg" "btn" "href"
          btnLink fs "btn btn-lg btn-ghost" "btn2" "href2"
      -- Di mode edit selalu ada (tersembunyi bila kosong) agar bisa diganti langsung.
      when (edit || not (T.null heroSrc)) $
        div_ [class_ "hero-media"] $
          img_ ([class_ "hero-img", src_ (if T.null heroSrc then "/asset/img/placeholder.svg" else heroSrc), alt_ (fieldOf "alt" fs)] <> [makeAttribute "fetchpriority" "high" | not edit])
  "head" -> section $ do
    txt fs p_ [class_ "eyebrow"] "eyebrow" "Label kecil"
    txt fs h1_ [] "title" "Judul halaman"
    txt fs p_ [class_ "lead ml"] "text" "Keterangan singkat"
  "text" -> section $ div_ [class_ "pb-narrow"] $ do
    txt fs h2_ [] "title" "Judul bagian"
    txt fs div_ [class_ "pb-body ml"] "text" "Tulis paragraf di sini"
  "features" -> sectionWith (cardClasses fs) $ do
    sectionHead fs
    when edit $
      span_
        ( [ class_ "pb-chip", data_ "variant" "style:fs-,cols:fc-,shape:sh-,ishape:is-"
          , data_ "fa" "style:data-style,cols:data-cols,shape:data-shape,ishape:data-ishape"
          ]
            <> [data_ k (maybe "" fst (lookup k cardVariants) `orIfBlank` fieldOf k fs) | k <- ["style", "cols", "shape", "ishape"]]
        )
        (icon "fa-solid fa-palette" >> " Gaya kartu")
    itemsOf "pb-grid" "pb-card" $ \it -> do
      ico it
      txt it h3_ [] "title" "Judul kartu"
      txt it p_ [class_ "ml"] "text" "Keterangan kartu"
  "split" -> section $ div_ [class_ "split"] $ do
    div_ [class_ "split-media"] (pic fs "split-img")
    div_ [class_ "split-body"] $ do
      txt fs p_ [class_ "eyebrow"] "eyebrow" "Label kecil"
      txt fs h2_ [] "title" "Judul bagian"
      txt fs div_ [class_ "pb-body ml"] "text" "Isi teks"
      actions (btnLink fs "btn" "btn" "href")
  "stats" -> section $
    itemsOf "pb-stats-grid" "pb-stat" $ \it -> do
      txt it strong_ [] "value" "100+"
      txt it span_ [] "label" "Keterangan"
  "cta" -> section $ div_ [class_ "cta-box"] $ do
    div_ $ do
      txt fs h2_ [] "title" "Judul ajakan"
      txt fs p_ [class_ "ml"] "text" "Kalimat pendukung"
    btnLink fs "btn btn-lg btn-light" "btn" "href"
  "branches" -> section $ do
    sectionHead fs
    itemsOf "pb-grid" "pb-card branch" $ \it -> do
      div_ [class_ "branch-top"] $ do
        span_ [class_ "pb-ico"] (icon "fa-solid fa-building")
        txt it h3_ [] "title" "Nama cabang"
      infoLine it "fa-solid fa-location-dot" "address" "Alamat"
      infoLine it "fa-solid fa-phone" "phone" "Telepon"
      infoLine it "fa-regular fa-clock" "hours" "Jam buka"
  "posts" -> section $ do
    sectionHead fs
    when edit $
      span_ [class_ "pb-chip", data_ "fa" "count:data-count", data_ "count" countT, data_ "label" "Jumlah artikel: "] $
        toHtml ("Jumlah artikel: " <> countT)
    let shown = take (fromMaybe 3 (readInt countT)) posts
    if null shown
      then when edit (emptyState "Belum ada artikel terbit.")
      else div_ [class_ "pb-grid pb-posts-grid"] (mapM_ postTile shown)
    div_ [class_ "pb-center"] (btnLink fs "btn btn-ghost" "btn" "href")
  "contact" -> section $ div_ [class_ "contact"] $ do
    div_ [class_ "contact-body"] $ do
      txt fs p_ [class_ "eyebrow"] "eyebrow" "Label kecil"
      txt fs h2_ [] "title" "Judul"
      txt fs p_ [class_ "ml"] "text" "Keterangan"
      let email = fieldOf "email" fs
          label = fieldOf "btn" fs
      if edit
        then a_ [class_ "btn btn-lg", href_ ("mailto:" <> email), data_ "f" "btn", data_ "ph" "Teks tombol"] (toHtml label)
        else unless (isBlank label || isBlank email) $
          a_ [class_ "btn btn-lg", href_ ("mailto:" <> T.strip email)] (icon "fa-regular fa-envelope" >> toHtml label)
    div_ [class_ "contact-list"] $ do
      infoCard "fa-solid fa-location-dot" "Alamat" "address"
      infoCard "fa-solid fa-phone" "Telepon" "phone"
      infoCard "fa-regular fa-envelope" "Email" "email"
      infoCard "fa-regular fa-clock" "Jam kerja" "hours"
  "slider" -> section $
    div_ [class_ "slider", data_ "slider" "", data_ "interval" intervalT, role_ "region", makeAttribute "aria-roledescription" "carousel", makeAttribute "aria-label" "Slider"] $ do
      itemsOfWith [data_ "axis" "x"] "slides" "slide" $ \it -> do
        let src = fieldOf "src" it
        if edit
          then do
            img_ [class_ "slide-bg", src_ (if T.null src then "/asset/img/placeholder.svg" else src), alt_ (fieldOf "alt" it)]
            span_ [class_ "pb-chip slide-chip", data_ "fa" "src:data-src,alt:data-alt", data_ "src" src, data_ "alt" (fieldOf "alt" it)] $
              icon "fa-regular fa-image" >> " Ganti gambar"
          else unless (T.null src) $ img_ [class_ "slide-bg", src_ src, alt_ (fieldOf "alt" it), makeAttribute "loading" "lazy"]
        div_ [class_ "slide-shade"] ""
        div_ [class_ "slide-body"] $ do
          txt it p_ [class_ "eyebrow"] "eyebrow" "Label kecil"
          txt it h2_ [] "title" "Judul slide"
          txt it p_ [class_ "ml"] "text" "Keterangan slide"
          actions $ do
            btnLink it "btn btn-lg btn-light" "btn" "href"
            btnLink it "btn btn-lg btn-glass" "btn2" "href2"
      button_ [type_ "button", class_ "slider-nav prev", data_ "slide-prev" "", makeAttribute "aria-label" "Slide sebelumnya"] (icon "fa-solid fa-chevron-left")
      button_ [type_ "button", class_ "slider-nav next", data_ "slide-next" "", makeAttribute "aria-label" "Slide berikutnya"] (icon "fa-solid fa-chevron-right")
      div_ [class_ "slider-dots", data_ "slider-dots" ""] ""
      when edit $
        span_ [class_ "pb-chip slider-interval", data_ "fa" "interval:data-interval", data_ "interval" intervalT, data_ "label" "Ganti otomatis: ", data_ "suffix" " detik"] $
          toHtml ("Ganti otomatis: " <> intervalT <> " detik")
  "image" -> section $ figure_ [class_ "pb-figure"] $ do
    pic fs "pb-img"
    txt fs figcaption_ [] "caption" "Keterangan gambar (opsional)"
  "testimonials" -> section $ do
    sectionHead fs
    itemsOf "pb-grid testi-grid" "pb-card testi" $ \it -> do
      stars it
      txt it blockquote_ [class_ "testi-quote ml"] "text" "Tulis testimoni di sini"
      div_ [class_ "testi-person"] $ do
        let src = fieldOf "src" it
        if edit || not (T.null src)
          then pic it "testi-avatar"
          else span_ [class_ "testi-avatar testi-initial", makeAttribute "aria-hidden" "true"] (toHtml (T.take 1 (fieldOf "title" it)))
        div_ $ do
          txt it strong_ [] "title" "Nama"
          txt it span_ [class_ "muted"] "role" "Jabatan / perusahaan"
  "faq" -> section $ div_ [class_ "pb-narrow"] $ do
    sectionHead fs
    if edit
      then itemsOf "faq-list" "faq-item faq-edit" $ \it -> do
        txt it h3_ [class_ "faq-q"] "title" "Pertanyaan"
        txt it div_ [class_ "faq-a ml"] "text" "Jawaban"
      else do
        let qa = [(q, a) | it <- bItems b, let q = fieldOf "title" it, let a = fieldOf "text" it, not (isBlank q)]
        div_ [class_ "faq-list"] $
          forM_ qa $ \(q, a) ->
            details_ [class_ "faq-item"] $ do
              summary_ [class_ "faq-q"] $ do
                span_ (toHtml q)
                i_ [class_ "fa-solid fa-plus faq-icon", makeAttribute "aria-hidden" "true"] ""
              div_ [class_ "faq-a ml"] (toHtml a)
        -- Data terstruktur agar Google bisa menampilkan tanya-jawab di hasil pencarian.
        unless (null qa) $
          script_ [type_ "application/ld+json"] (faqJsonLd qa)
  "pricing" -> section $ do
    sectionHead fs
    itemsOf "pb-grid price-grid" "pb-card price-card" $ \it -> do
      let featured = fieldOf "featured" it == "1"
      if edit
        then span_ [class_ "pb-chip price-chip", data_ "fa" "featured:data-featured", data_ "featured" (if featured then "1" else "")] (icon "fa-solid fa-star" >> " Sorot paket")
        else when featured $ span_ [class_ "price-flag", data_ "featured" "1"] ""
      txt it h3_ [class_ "price-name"] "title" "Nama paket"
      div_ [class_ "price-amount"] $ do
        txt it strong_ [] "price" "Rp 0"
        txt it span_ [class_ "muted"] "period" "/bulan"
      txt it p_ [class_ "muted ml price-desc"] "text" "Deskripsi singkat"
      if edit
        then txt it div_ [class_ "price-features ml"] "features" "Satu fitur per baris"
        else do
          let feats = filter (not . isBlank) (T.lines (fieldOf "features" it))
          unless (null feats) $
            ul_ [class_ "price-features"] $ forM_ feats $ \l -> li_ (icon "fa-solid fa-check" >> span_ (toHtml (T.strip l)))
      btnLink it ("btn price-btn" <> if featured then "" else " btn-ghost") "btn" "href"
  "team" -> section $ do
    sectionHead fs
    itemsOf "pb-grid team-grid" "team-card" $ \it -> do
      pic it "team-photo"
      txt it h3_ [] "title" "Nama"
      txt it p_ [class_ "team-role"] "role" "Jabatan"
      txt it p_ [class_ "muted ml team-bio"] "text" "Bio singkat (opsional)"
  "gallery" -> section $ do
    sectionHead fs
    itemsOf "gallery-grid" "gallery-item" $ \it -> do
      pic it "gallery-img"
      txt it span_ [class_ "gallery-cap"] "caption" "Keterangan (opsional)"
  "logos" -> section $ do
    txt fs p_ [class_ "logos-title"] "title" "Judul kecil (opsional)"
    itemsOf "logo-row" "logo-item" (`pic` "logo-img")
  "video"
    | not edit && isNothing (youtubeId (fieldOf "url" fs)) -> mempty
    | otherwise -> section $ do
    sectionHead fs
    let url = fieldOf "url" fs
    div_ [class_ "video-box"] $ do
      when edit $
        span_ [class_ "pb-chip video-chip", data_ "fa" "url:data-url", data_ "url" url, data_ "video" ""] (icon "fa-brands fa-youtube" >> " URL video YouTube")
      div_ [class_ "video-frame", data_ "video-frame" ""] $ case youtubeId url of
        Just vid ->
          iframe_
            [ src_ ("https://www.youtube-nocookie.com/embed/" <> vid)
            , title_ (let t = fieldOf "title" fs in if isBlank t then "Video" else t)
            , makeAttribute "allow" "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
            , makeAttribute "allowfullscreen" ""
            , makeAttribute "loading" "lazy"
            , makeAttribute "referrerpolicy" "strict-origin-when-cross-origin"
            ]
            ""
        Nothing -> when edit $ div_ [class_ "video-empty"] (icon "fa-brands fa-youtube" >> span_ "Klik “URL video YouTube” lalu tempel tautan videonya")
  "code"
    | not edit && all (isBlank . (`fieldOf` fs)) ["html", "css", "js"] -> mempty
    | otherwise -> section $ do
        when edit $
          span_
            [ class_ "pb-chip", data_ "fa" "html:data-html,css:data-css,js:data-js"
            , data_ "html" (fieldOf "html" fs), data_ "css" (fieldOf "css" fs), data_ "js" (fieldOf "js" fs)
            ]
            (icon "fa-solid fa-code" >> " Edit HTML, CSS & JavaScript")
        codeFrame (fieldOf "html" fs) (fieldOf "css" fs) (fieldOf "js" fs)
  _ -> mempty
  where
    fs = bFields b
    countT = let v = fieldOf "count" fs in if T.null v then "3" else v
    intervalT = let v = fieldOf "interval" fs in if T.null v then "5" else v
    heroSrc = fieldOf "src" fs
    heroLayout = let v = fieldOf "layout" fs in if v `elem` ["left", "bg"] then v else "right"
    heroCls = "hero-" <> heroLayout <> if T.null heroSrc then "" else " has-img"

    section :: Html () -> Html ()
    section = sectionWith ""

    sectionWith :: Text -> Html () -> Html ()
    sectionWith extra inner =
      section_ ([class_ (T.strip ("pb pb-" <> bType b <> " " <> extra))] <> [data_ "block" (bType b) | edit]) $
        div_ [class_ "wrap"] inner

    actions :: Html () -> Html ()
    actions = div_ [class_ "pb-actions"]

    -- Teks yang bisa diedit di tempat. Di mode lihat, isian kosong tidak dirender.
    txt :: Fields -> ([Attribute] -> Html () -> Html ()) -> [Attribute] -> Text -> Text -> Html ()
    txt m el attrs name ph
      | edit = el (attrs <> [data_ "f" name, data_ "ph" ph]) (toHtml v)
      | isBlank v = mempty
      | otherwise = el attrs (toHtml v)
      where
        v = fieldOf name m

    btnLink :: Fields -> Text -> Text -> Text -> Html ()
    btnLink m cls lf hf
      | edit = a_ [class_ cls, href_ h, data_ "f" lf, data_ "fa" (hf <> ":href"), data_ "ph" "Teks tombol"] (toHtml l)
      | isBlank l = mempty
      | otherwise = a_ [class_ cls, href_ (if T.null h then "#" else h)] (toHtml l)
      where
        l = fieldOf lf m
        h = fieldOf hf m

    pic :: Fields -> Text -> Html ()
    pic m cls
      | edit = img_ [class_ cls, src_ (if T.null src then "/asset/img/placeholder.svg" else src), alt_ alt, data_ "fa" "src:src,alt:alt"]
      | T.null src = mempty
      | otherwise = img_ [class_ cls, src_ src, alt_ alt, makeAttribute "loading" "lazy"]
      where
        src = fieldOf "src" m
        alt = fieldOf "alt" m

    -- Rating bintang testimoni (1–5). Di mode edit berupa chip yang bisa diklik.
    stars :: Fields -> Html ()
    stars it
      | edit = span_ [class_ "pb-chip stars-chip", data_ "fa" "rating:data-rating", data_ "rating" r, data_ "label" "Bintang: ", data_ "suffix" " / 5"] (toHtml ("Bintang: " <> r <> " / 5"))
      | otherwise =
          div_ [class_ "stars", role_ "img", makeAttribute "aria-label" (r <> " dari 5 bintang")] $
            forM_ [1 .. 5 :: Int] $ \i -> i_ [class_ (if i <= n then "fa-solid fa-star" else "fa-regular fa-star"), makeAttribute "aria-hidden" "true"] ""
      where
        n = maybe 5 (max 1 . min 5) (readInt (fieldOf "rating" it))
        r = tshow n

    ico :: Fields -> Html ()
    ico m
      | edit = span_ [class_ "pb-ico", data_ "fa" "icon:data-icon", data_ "icon" v] (i_ [class_ v] "")
      | T.null v = mempty
      | otherwise = span_ [class_ "pb-ico"] (icon v)
      where
        v = fieldOf "icon" m

    sectionHead :: Fields -> Html ()
    sectionHead m =
      when (edit || any (not . isBlank . (`fieldOf` m)) ["eyebrow", "title", "text"]) $
        div_ [class_ "pb-heading"] $ do
          txt m p_ [class_ "eyebrow"] "eyebrow" "Label kecil"
          txt m h2_ [] "title" "Judul bagian"
          txt m p_ [class_ "pb-sub ml"] "text" "Keterangan (opsional)"

    -- Daftar item. Di mode edit ikut disertakan template item kosong untuk "Tambah item".
    itemsOf :: Text -> Text -> (Fields -> Html ()) -> Html ()
    itemsOf = itemsOfWith []

    itemsOfWith :: [Attribute] -> Text -> Text -> (Fields -> Html ()) -> Html ()
    itemsOfWith extra listCls itemCls one =
      div_ ([class_ listCls] <> extra <> [data_ "items" "" | edit]) $ do
        forM_ (bItems b) wrapItem
        when edit $ term "template" [data_ "item-tpl" ""] (wrapItem M.empty)
      where
        wrapItem :: Fields -> Html ()
        wrapItem it = div_ ([class_ itemCls] <> [data_ "item" "" | edit]) (one it)

    infoLine :: Fields -> Text -> Text -> Text -> Html ()
    infoLine m ic name ph =
      when (edit || not (isBlank (fieldOf name m))) $
        p_ [class_ "info-line"] $ do
          icon ic
          txt m span_ [class_ "ml"] name ph

    infoCard :: Text -> Text -> Text -> Html ()
    infoCard ic label name =
      when (edit || not (isBlank (fieldOf name fs))) $
        div_ [class_ "info-card"] $ do
          span_ [class_ "pb-ico"] (icon ic)
          div_ $ do
            small_ [class_ "muted"] (toHtml label)
            txt fs div_ [class_ "info-value ml"] name label

postTile :: Post -> Html ()
postTile p = article_ [class_ "pb-card post-tile"] $ do
  div_ [class_ "post-meta"] $ do
    forM_ (postCategoryName p) $ \n -> span_ [class_ "tag"] (toHtml n)
    span_ (toHtml (fmtDate (fromMaybe (postCreated p) (postPublished p))))
  h3_ $ a_ [href_ ("/post/" <> postSlug p)] (toHtml (postTitle p))
  p_ [class_ "muted"] $
    toHtml (if isBlank (postExcerpt p) then plainExcerpt 140 (postBody p) else postExcerpt p)
  a_ [href_ ("/post/" <> postSlug p), class_ "read-more"] "Baca selengkapnya →"

-- Perangkat editor -------------------------------------------------------------------------

editorBar :: Ctx -> Page -> Html ()
editorBar c pg =
  div_ [class_ "pb-bar"] $ do
    input_ [type_ "hidden", id_ "pb-csrf", value_ (cCsrf c)]
    div_ [class_ "pb-bar-title"] $ do
      icon "fa-solid fa-pen-ruler"
      span_ $ do
        "Mengedit "
        strong_ (toHtml (pageTitle pg))
    span_ [id_ "pb-status", class_ "pb-status", role_ "status"] "Belum ada perubahan"
    div_ [class_ "pb-bar-actions"] $ do
      a_ [href_ "/admin/pages", class_ "btn btn-ghost btn-sm"] (icon "fa-solid fa-table-list" >> "Semua halaman")
      a_ [href_ (pagePath (pageSlug pg)), class_ "btn btn-ghost btn-sm"] (icon "fa-solid fa-eye" >> "Selesai")
      button_ [type_ "button", id_ "pb-save", class_ "btn btn-sm"] (icon "fa-solid fa-floppy-disk" >> "Simpan")

editorPalette :: NavStyle -> [Post] -> Html ()
editorPalette nav posts = do
  aside_ [class_ "pb-palette", data_ "palette" ""] $ do
    h2_ "Tambah blok"
    p_ [class_ "muted"] "Seret blok ke halaman, atau klik untuk menambah di paling bawah."
    div_ [class_ "pb-palette-list"] $
      forM_ blockSpecs $ \sp ->
        button_ [type_ "button", class_ "pb-pal", makeAttribute "draggable" "true", data_ "type" (spType sp)] $ do
          icon (spIcon sp)
          span_ (toHtml (spLabel sp))
    navPanel nav
    div_ [class_ "pb-help"] $ do
      p_ (icon "fa-solid fa-i-cursor" >> " Klik teks untuk mengubahnya.")
      p_ (icon "fa-solid fa-link" >> " Klik tombol, gambar, atau ikon untuk mengganti tautan/gambar/ikon.")
      p_ (icon "fa-solid fa-grip-vertical" >> " Seret pegangan untuk memindahkan blok atau item.")
      p_ (icon "fa-regular fa-keyboard" >> " Ctrl+S untuk menyimpan.")
  forM_ blockSpecs $ \sp ->
    term "template" [data_ "tpl" (spType sp)] (renderBlock True posts (spDefault sp))
  script_ [src_ "/static/builder.js?v=13", defer_ "defer"] ("" :: Text)

-- | Pengaturan warna & jarak navigasi atas/bawah. Nilai dibaca static/builder.js,
-- dipratinjau langsung, dan ikut terkirim saat Simpan.
navPanel :: NavStyle -> Html ()
navPanel s =
  div_ [class_ "pb-nav-section", data_ "nav-panel" ""] $ do
    h2_ "Tampilan situs"
    p_ [class_ "muted"] "Ukuran website serta warna & jarak menu atas dan bawah. Berlaku di semua halaman."
    forM_ navGroups $ \g ->
      details_ [class_ "pb-nav", data_ "nav-group" (ngId g)] $ do
        summary_ $ do
          icon (ngIcon g)
          span_ (toHtml (ngTitle g))
          i_ [class_ "fa-solid fa-chevron-down pb-nav-caret", makeAttribute "aria-hidden" "true"] ""
        div_ [class_ "pb-nav-body"] $ do
          forM_ (ngFields g) row
          button_ [type_ "button", class_ "btn btn-ghost btn-sm", data_ "nav-reset" (ngId g)] $
            icon "fa-solid fa-rotate-left" >> "Kembalikan bawaan"
  where
    row :: NavField -> Html ()
    row f = do
      let k = nfKey f
          cur = M.lookup k s
          fid = "nav-" <> k
      case nfKind f of
        NavToggle def ->
          label_ [class_ "switch-row pb-nav-switch", for_ fid] $ do
            span_ (toHtml (nfLabel f))
            input_ ([type_ "checkbox", id_ fid, data_ "nav-key" k, data_ "default" (if def then "1" else "0")] <> [checked_ | maybe def (== "1") cur])
        NavSelect def opts ->
          label_ [class_ "pb-nav-select", for_ fid] $ do
            span_ (toHtml (nfLabel f))
            select_ [id_ fid, data_ "nav-key" k, data_ "default" def] $
              forM_ opts $ \(v, l) -> option_ ([value_ v] <> [selected_ "selected" | v == fromMaybe def cur]) (toHtml l)
        _ -> slider f k cur fid
    slider :: NavField -> Text -> Maybe Text -> Text -> Html ()
    slider f k cur fid =
      div_ ([class_ "pb-nav-row"] <> [data_ "unset" "" | isNothing cur]) $ do
        label_ [for_ fid] (toHtml (nfLabel f))
        case nfKind f of
          NavColor ->
            input_ [type_ "color", id_ fid, data_ "nav-key" k, value_ (fromMaybe (colorHint k) cur)]
          NavRange lo hi def unit ->
            input_
              [ type_ "range", id_ fid, data_ "nav-key" k
              , min_ (tshow lo), max_ (tshow hi), value_ (fromMaybe (tshow def) cur)
              , data_ "default" (tshow def), data_ "unit" unit
              ]
          _ -> mempty
        span_ [class_ "pb-nav-val", data_ "nav-out" ""] $
          toHtml (maybe "bawaan" (<> unitOf (nfKind f)) cur)
        button_ [type_ "button", class_ "pb-nav-clear", data_ "nav-clear" k, title_ "Pakai bawaan", makeAttribute "aria-label" "Pakai bawaan"] $
          icon "fa-solid fa-xmark"
    unitOf :: NavKind -> Text
    unitOf (NavRange _ _ _ u) = u
    unitOf _ = ""
    -- Warna awal pemilih warna bila belum diatur (mendekati tampilan bawaan).
    colorHint :: Text -> Text
    colorHint k
      | "_bg" `T.isSuffixOf` k = "#ffffff"
      | "_text" `T.isSuffixOf` k = "#1c2524"
      | otherwise = "#0f766e"

-- Kode custom ---------------------------------------------------------------------------

-- | HTML/CSS/JS buatan pengguna dijalankan di iframe sandbox (origin terpisah), jadi
-- skripnya tidak bisa membaca sesi atau memakai hak akses admin. Tinggi iframe
-- mengikuti isinya lewat postMessage (lihat static/app.js).
codeFrame :: Text -> Text -> Text -> Html ()
codeFrame html css js =
  iframe_
    [ class_ "pb-code-frame"
    , data_ "code-frame" ""
    , title_ "Kode custom"
    , makeAttribute "sandbox" "allow-scripts allow-forms allow-popups allow-popups-to-escape-sandbox allow-top-navigation-by-user-activation"
    , makeAttribute "srcdoc" (codeDoc html css js)
    ]
    ""

-- | Sama persis dengan codeDoc() di static/builder.js (dipakai untuk pratinjau).
codeDoc :: Text -> Text -> Text -> Text
codeDoc html css js =
  T.concat
    [ "<!doctype html><html><head><meta charset=\"utf-8\">"
    , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
    , "<base target=\"_top\">"
    , "<style>html,body{margin:0}body{font-family:Nunito,system-ui,-apple-system,\"Segoe UI\",Roboto,sans-serif}</style>"
    , "<style>", css, "</style></head><body>", html
    , "<script>", js, "</script>"
    , "<script>(function(){function s(){parent.postMessage({dcmsCodeHeight:Math.ceil(document.documentElement.getBoundingClientRect().height)},\"*\")}"
    , "if(window.ResizeObserver)new ResizeObserver(s).observe(document.documentElement);addEventListener(\"load\",s);s()})();</script>"
    , "</body></html>"
    ]

-- | Nilai isian, atau bawaan bila kosong.
orIfBlank :: Text -> Text -> Text
orIfBlank def v = if T.null (T.strip v) then def else v

-- | JSON-LD FAQPage. "<" di-escape agar isi tidak bisa menutup tag <script>.
faqJsonLd :: [(Text, Text)] -> Text
faqJsonLd qa =
  T.replace "<" "\\u003c" . TE.decodeUtf8 . BL.toStrict . encode $
    object
      [ "@context" .= ("https://schema.org" :: Text)
      , "@type" .= ("FAQPage" :: Text)
      , "mainEntity"
          .= [ object
                 [ "@type" .= ("Question" :: Text)
                 , "name" .= q
                 , "acceptedAnswer" .= object ["@type" .= ("Answer" :: Text), "text" .= a]
                 ]
             | (q, a) <- qa
             ]
      ]
