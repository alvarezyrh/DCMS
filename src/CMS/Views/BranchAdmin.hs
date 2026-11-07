-- | Menu Cabang di dashboard: daftar, tambah, ubah, hapus cabang/outlet.
-- Datanya tampil di halaman situs /branch lewat blok "Direktori cabang".
module CMS.Views.BranchAdmin (branchesPage, branchFormPage) where

import Control.Monad (forM_, unless, when)
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Lucid.Base (makeAttribute)

import CMS.Types
import CMS.Util
import CMS.Views.Layout

-- | Daftar cabang. @all@ = semua cabang (untuk ringkasan & pilihan provinsi),
-- @shown@ = hasil pencarian/filter.
branchesPage :: Ctx -> User -> [Branch] -> [Branch] -> Text -> Text -> Html ()
branchesPage c u allBs shown q prov = adminLayout c u "branches" "Cabang" $ do
  let active = filter brActive allBs
      provs = nub (map brProvince allBs)
  div_ [class_ "branch-kpis"] $ do
    kpi "fa-solid fa-store" "Cabang aktif" (tshow (length active))
    kpi "fa-solid fa-map-location-dot" "Provinsi" (tshow (length (nub (map brProvince active))))
    kpi "fa-solid fa-city" "Kota" (tshow (length (nub (map brCity active))))
    kpi "fa-regular fa-eye-slash" "Nonaktif" (tshow (length allBs - length active))
  div_ [class_ "toolbar"] $ do
    form_ [method_ "get", action_ "/admin/branches", class_ "filters"] $ do
      input_ [type_ "search", name_ "q", value_ q, placeholder_ "Cari nama, kota, alamat, telepon…"]
      select_ [name_ "prov"] $ do
        option_ [value_ ""] "Semua provinsi"
        forM_ provs $ \p -> option_ ([value_ p] <> [selected_ "selected" | p == prov]) (toHtml p)
      button_ [type_ "submit", class_ "btn btn-ghost"] "Filter"
      unless (T.null q && T.null prov) $ a_ [href_ "/admin/branches", class_ "btn btn-ghost"] "Reset"
    div_ [class_ "row"] $ do
      a_ [href_ "/branch", target_ "_blank", rel_ "noopener", class_ "btn btn-ghost"] (icon "fa-solid fa-arrow-up-right-from-square" >> "Lihat halaman cabang")
      a_ [href_ "/admin/branches/new", class_ "btn"] "+ Tambah cabang"
  p_ [class_ "muted"] (toHtml (tshow (length shown) <> " cabang" <> if length shown /= length allBs then " (dari " <> tshow (length allBs) <> ")" else ""))
  if null shown
    then emptyState (if null allBs then "Belum ada cabang. Klik “Tambah cabang”." else "Tidak ada cabang yang cocok.")
    else div_ [class_ "table-wrap card"] $ table_ [class_ "table branch-table"] $ do
      thead_ $ tr_ $ do
        th_ [class_ "num"] "Urutan"
        th_ "Cabang"
        th_ "Kontak"
        th_ "Jam operasional"
        th_ "Status"
        th_ [class_ "right"] "Aksi"
      tbody_ $ forM_ shown $ \b -> tr_ ([class_ "is-off" | not (brActive b)]) $ do
        td_ [class_ "num mono"] (toHtml (tshow (brSort b)))
        td_ $ do
          a_ [href_ (editUrl b), class_ "strong"] (toHtml (brName b))
          br_ []
          small_ [class_ "muted"] (toHtml (T.intercalate " · " (filter (not . T.null) [brCity b, brProvince b])))
        td_ $ do
          unless (isBlank (brPhone b)) $ div_ (icon "fa-solid fa-phone" >> " " >> toHtml (brPhone b))
          unless (isBlank (brPhone2 b)) $ div_ [class_ "muted"] (toHtml (brPhone2 b))
          when (isBlank (brPhone b)) $ span_ [class_ "muted"] "—"
        td_ (if isBlank (brHours b) then span_ [class_ "muted"] "—" else toHtml (brHours b))
        td_ (if brActive b then span_ [class_ "badge badge-published"] "Aktif" else span_ [class_ "badge badge-draft"] "Nonaktif")
        td_ [class_ "right actions"] $ do
          unless (isBlank (brMapsUrl b)) $
            a_ [href_ (brMapsUrl b), target_ "_blank", rel_ "noopener", class_ "btn btn-ghost btn-sm", title_ "Buka di Google Maps"] (icon "fa-regular fa-map")
          a_ [href_ (editUrl b), class_ "btn btn-ghost btn-sm"] "Edit"
          form_ [method_ "post", action_ ("/admin/branches/" <> tshow (brId b) <> "/delete"), class_ "inline", data_ "confirm" ("Hapus cabang “" <> brName b <> "”?")] $ do
            csrfField c
            button_ [type_ "submit", class_ "btn btn-danger btn-sm"] "Hapus"
  where
    editUrl b = "/admin/branches/" <> tshow (brId b) <> "/edit"
    kpi :: Text -> Text -> Text -> Html ()
    kpi ic label val =
      div_ [class_ "card kpi"] $ do
        span_ [class_ "kpi-ico"] (icon ic)
        div_ $ do
          span_ [class_ "kpi-label"] (toHtml label)
          strong_ [class_ "kpi-value"] (toHtml val)

branchFormPage :: Ctx -> User -> Maybe Branch -> BranchInput -> [Text] -> Html ()
branchFormPage c u mB BranchInput {..} errs =
  adminLayout c u "branches" (if isJust mB then "Edit Cabang" else "Tambah Cabang") $ do
    p_ $ a_ [href_ "/admin/branches", class_ "muted"] "← Semua cabang"
    errorList errs
    form_ [method_ "post", action_ action, class_ "branch-form"] $ do
      csrfField c
      section_ [class_ "card stack"] $ do
        h3_ "Informasi cabang"
        div_ [class_ "form-grid"] $ do
          inputField "text" "Nama outlet" "name" biName [required_ "required", placeholder_ "mis. Pondok Indah", makeAttribute "maxlength" "120"]
          inputField "text" "Kota / wilayah" "city" biCity [required_ "required", placeholder_ "mis. Jakarta Selatan", makeAttribute "maxlength" "80"]
        field "Provinsi" "province" $
          select_ [id_ "province", name_ "province", required_ "required"] $ do
            option_ [value_ ""] "— Pilih provinsi —"
            forM_ (provinces ++ [biProvince | not (T.null biProvince), biProvince `notElem` provinces]) $ \p ->
              option_ ([value_ p] <> [selected_ "selected" | p == biProvince]) (toHtml p)
        textareaField "Alamat lengkap" "address" biAddress 3 [required_ "required", makeAttribute "maxlength" "400"]
        h3_ [class_ "settings-sub"] (icon "fa-solid fa-phone" >> " Kontak & jam")
        div_ [class_ "form-grid"] $ do
          inputField "tel" "Telepon / HP" "phone" biPhone [placeholder_ "0822-9999-5125", makeAttribute "inputmode" "tel"]
          inputField "tel" "Telepon kedua (opsional)" "phone2" biPhone2 [makeAttribute "inputmode" "tel"]
        inputField "tel" "Nomor WhatsApp (opsional)" "whatsapp" biWhatsapp [placeholder_ "Kosongkan = sama dengan telepon", makeAttribute "inputmode" "tel"]
        div_ [class_ "form-grid"] $ do
          inputField "text" "Jam operasional" "hours" biHours [placeholder_ "10.00–21.00 WIB", makeAttribute "maxlength" "120"]
          inputField "text" "Catatan" "note" biNote [placeholder_ "Buka setiap hari", makeAttribute "maxlength" "80"]
        h3_ [class_ "settings-sub"] (icon "fa-regular fa-map" >> " Peta")
        inputField "url" "Tautan petunjuk arah (Google Maps)" "maps_url" biMapsUrl [placeholder_ "https://maps.app.goo.gl/…"]
        hint "Di Google Maps: buka lokasi → Bagikan → Salin link."
        inputField "text" "Kata kunci peta" "maps_query" biMapsQuery [placeholder_ "mis. Raja Emas Pondok Indah", data_ "map-preview-q" ""]
        hint "Dipakai untuk peta pada tombol “Lihat peta”. Isi nama tempat seperti yang dicari di Google Maps."
        inputField "text" "Koordinat (latitude, longitude)" "coords" (maybe "" (\(la, lo) -> tshow la <> ", " <> tshow lo) ((,) <$> biLat <*> biLng)) [placeholder_ "-6.2088, 106.8456", makeAttribute "inputmode" "decimal"]
        hint "Untuk urutan “terdekat” di halaman cabang. Di Google Maps: klik kanan lokasi → klik angka koordinat untuk menyalin. Kosongkan untuk mengambil otomatis dari tautan peta lengkap."
        div_ [class_ "map-preview", data_ "map-preview" ""] $
          unless (isBlank biMapsQuery) $
            iframe_ [src_ (mapEmbed biMapsQuery), title_ "Pratinjau peta", makeAttribute "loading" "lazy", makeAttribute "referrerpolicy" "no-referrer-when-downgrade"] ""
        h3_ [class_ "settings-sub"] (icon "fa-solid fa-sliders" >> " Tampilan")
        div_ [class_ "form-grid"] $ do
          inputField "number" "Urutan tampil" "sort" (tshow biSort) [min_ "0", max_ "100000"]
          div_ [class_ "field"] $ do
            label_ "Status"
            checkboxField "Tampilkan di halaman cabang" "active" biActive
        hint "Urutan kecil tampil lebih dulu. Cabang dikelompokkan per provinsi mengikuti urutan ini."
        div_ [class_ "row"] $ do
          button_ [type_ "submit", class_ "btn"] (if isJust mB then "Simpan perubahan" else "Simpan cabang")
          a_ [href_ "/admin/branches", class_ "btn btn-ghost"] "Batal"
  where
    action = maybe "/admin/branches" (\b -> "/admin/branches/" <> tshow (brId b)) mB

mapEmbed :: Text -> Text
mapEmbed q = "https://maps.google.com/maps?q=" <> T.replace " " "+" (T.filter (`notElem` ("&#?\"<>" :: String)) q) <> "&output=embed"
