-- | Halaman situs publik + login/register.
module CMS.Views.Public where

import Control.Monad (forM_, when)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Lucid
import Lucid.Base (makeAttribute)

import CMS.Seo (effectiveDesc, firstImage)
import CMS.Web (siteOrigin)

import CMS.Types
import CMS.Util
import CMS.Views.Layout

postMeta :: Post -> Html ()
postMeta p = div_ [class_ "post-meta"] $ do
  forM_ ((,) <$> postCategoryName p <*> postCategorySlug p) $ \(n, s) ->
    a_ [href_ ("/category/" <> s), class_ "tag"] (toHtml n)
  span_ (toHtml (fmtDate (fromMaybe (postCreated p) (postPublished p))))
  span_ (toHtml ("oleh " <> postAuthorName p))

postCard :: Post -> Html ()
postCard p = article_ [class_ "card post-card"] $ do
  postMeta p
  h2_ $ a_ [href_ ("/post/" <> postSlug p)] (toHtml (postTitle p))
  p_ [class_ "excerpt"] $
    toHtml (if isBlank (postExcerpt p) then plainExcerpt 220 (postBody p) else postExcerpt p)
  a_ [href_ ("/post/" <> postSlug p), class_ "read-more"] "Baca selengkapnya →"

-- | Beranda, hasil pencarian, dan arsip kategori memakai halaman yang sama.
listingPage :: Ctx -> [Category] -> Maybe Category -> [Post] -> Text -> Int -> Int -> Html ()
listingPage c cats mCat posts q page pages =
  publicLayout c cats title $ do
    section_ [class_ "hero"] $ case mCat of
      Just k -> do
        p_ [class_ "eyebrow"] "Kategori"
        h1_ (toHtml (catName k))
        when (not (isBlank (catDesc k))) $ p_ [class_ "lead"] (toHtml (catDesc k))
      Nothing
        | not (isBlank q) -> do
            p_ [class_ "eyebrow"] "Pencarian"
            h1_ (toHtml ("“" <> q <> "”"))
        | otherwise -> do
            h1_ (toHtml (setSiteName (cSettings c)))
            p_ [class_ "lead"] (toHtml (setTagline (cSettings c)))
    if null posts
      then emptyState "Belum ada artikel yang cocok."
      else div_ [class_ "post-list"] (mapM_ postCard posts)
    pagination base [("q", q)] page pages
  where
    title = maybe (if isBlank q then "Beranda" else "Cari: " <> q) catName mCat
    base = maybe "/" (("/category/" <>) . catSlug) mCat

postPage :: Ctx -> [Category] -> Post -> Html ()
postPage c cats p = publicLayoutMeta c cats meta $
  article_ [class_ "post-full"] $ do
    postMeta p
    h1_ (toHtml (postTitle p))
    when (not (isBlank (postExcerpt p))) $ p_ [class_ "lead"] (toHtml (postExcerpt p))
    forM_ (cUser c) $ \u ->
      when (canEditPost u p) $
        p_ $ a_ [href_ ("/admin/posts/" <> tshow (postId p) <> "/edit"), class_ "btn btn-ghost btn-sm"] "✎ Edit artikel"
    div_ [class_ "prose"] (toHtmlRaw (renderMarkdown (postBody p)))
    p_ [class_ "back"] $ a_ [href_ "/blog"] "← Kembali ke blog"
  where
    s = postSeo p
    site = setSiteName (cSettings c)
    desc = effectiveDesc (postExcerpt p) (postBody p) s
    image = if isBlank (seoImage s) then firstImage (postBody p) else Just (seoImage s)
    published = fromMaybe (postCreated p) (postPublished p)
    origin = siteOrigin c
    absolute u = if "/" `T.isPrefixOf` u && not ("//" `T.isPrefixOf` u) then origin <> u else u
    meta =
      PageMeta
        { pmTitle = if isBlank (seoTitle s) then postTitle p <> " · " <> site else T.strip (seoTitle s)
        , pmDesc = Just desc
        , pmImage = image
        , pmType = "article"
        , pmNoindex = seoNoindex s
        , pmExtra = do
            meta_ [makeAttribute "property" "article:published_time", content_ (isoTime published)]
            meta_ [makeAttribute "property" "article:modified_time", content_ (isoTime (postUpdated p))]
            forM_ (postCategoryName p) $ \k -> meta_ [makeAttribute "property" "article:section", content_ k]
            script_ [type_ "application/ld+json"] (jsonLd articleLd)
        }
    articleLd =
      object $
        [ "@context" .= ("https://schema.org" :: Text)
        , "@type" .= ("BlogPosting" :: Text)
        , "headline" .= T.take 110 (postTitle p)
        , "description" .= desc
        , "datePublished" .= isoTime published
        , "dateModified" .= isoTime (postUpdated p)
        , "author" .= object ["@type" .= ("Person" :: Text), "name" .= postAuthorName p]
        , "publisher" .= object ["@type" .= ("Organization" :: Text), "name" .= site]
        , "mainEntityOfPage" .= (origin <> "/post/" <> postSlug p)
        ]
          ++ ["image" .= absolute i | Just i <- [image]]
          ++ ["keywords" .= seoKeyword s | not (isBlank (seoKeyword s))]

-- | "2026-11-07 10:00:00" (UTC dari SQLite) -> "2026-11-07T10:00:00Z".
isoTime :: Text -> Text
isoTime t = T.replace " " "T" (T.strip t) <> if "Z" `T.isSuffixOf` t then "" else "Z"

-- | JSON untuk <script type="application/ld+json">; "<" di-escape agar aman.
jsonLd :: Value -> Text
jsonLd = T.replace "<" "\\u003c" . TE.decodeUtf8 . BL.toStrict . encode

loginPage :: Ctx -> [Text] -> Text -> Text -> Html ()
loginPage c errs login next = authLayout c "Masuk" $ do
  h1_ "Masuk"
  p_ [class_ "muted"] "Masuk ke dashboard untuk mengelola konten."
  errorList errs
  form_ [method_ "post", action_ "/login", class_ "stack"] $ do
    csrfField c
    input_ [type_ "hidden", name_ "next", value_ next]
    inputField "text" "Username atau email" "login" login [required_ "required", autofocus_, ac "username"]
    inputField "password" "Password" "password" "" [required_ "required", ac "current-password"]
    button_ [type_ "submit", class_ "btn btn-block"] "Masuk"
  when (setAllowRegister (cSettings c)) $
    p_ [class_ "muted center"] $ do
      "Belum punya akun? "
      a_ [href_ "/register"] "Daftar"

registerPage :: Ctx -> [Text] -> (Text, Text, Text) -> Html ()
registerPage c errs (uname, email, display) = authLayout c "Daftar" $ do
  h1_ "Buat akun"
  p_ [class_ "muted"] "Akun baru mendapat peran User: bisa menulis artikel yang akan direview Editor."
  errorList errs
  form_ [method_ "post", action_ "/register", class_ "stack"] $ do
    csrfField c
    inputField "text" "Username" "username" uname [required_ "required", pattern_ "[A-Za-z0-9_]{3,30}", ac "username"]
    hint "3–30 karakter: huruf, angka, garis bawah."
    inputField "email" "Email" "email" email [required_ "required", ac "email"]
    inputField "text" "Nama tampilan" "display_name" display []
    inputField "password" "Password" "password" "" [required_ "required", minLen 8, ac "new-password"]
    inputField "password" "Ulangi password" "password2" "" [required_ "required", minLen 8, ac "new-password"]
    button_ [type_ "submit", class_ "btn btn-block"] "Daftar"
  p_ [class_ "muted center"] $ do
    "Sudah punya akun? "
    a_ [href_ "/login"] "Masuk"
