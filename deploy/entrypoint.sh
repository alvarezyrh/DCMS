#!/bin/sh
# Siapkan folder data (volume /data) lalu jalankan DCMS.
set -eu
mkdir -p "$CMS_ASSETS"
# Gambar contoh dipakai halaman bawaan; salin bila volume masih kosong.
[ -f "$CMS_ASSETS/placeholder.svg" ] || cp /app/defaults/placeholder.svg "$CMS_ASSETS/placeholder.svg"
exec /app/DCMS
