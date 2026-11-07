#!/bin/sh
# Siapkan folder data (volume /data) lalu jalankan DCMS.
set -eu

fail() {
  echo "GALAT: $1" >&2
  echo "  Aplikasi berjalan sebagai user dcms (uid $(id -u)), tetapi $2 tidak bisa ditulis." >&2
  echo "  Perbaikan: pasang volume dengan opsi :U agar Podman menyesuaikan pemiliknya, mis." >&2
  echo "    podman run ... -v dcms-data:/data:U ...        (bind mount: -v /folder:/data:U,Z)" >&2
  echo "  atau di host: sudo chown -R 10001:10001 <folder-data>" >&2
  exit 1
}

# Cek izin tulis lebih dulu agar error-nya jelas (bukan "readonly database" dari SQLite).
DATA_DIR=$(dirname "$CMS_DB")
touch "$DATA_DIR/.write-test" 2>/dev/null && rm -f "$DATA_DIR/.write-test" \
  || fail "folder data tidak bisa ditulis" "$DATA_DIR"
if [ -e "$CMS_DB" ] && [ ! -w "$CMS_DB" ]; then
  fail "database tidak bisa ditulis (attempt to write a readonly database)" "$CMS_DB"
fi
for f in "$CMS_DB-wal" "$CMS_DB-shm"; do
  if [ -e "$f" ] && [ ! -w "$f" ]; then fail "berkas SQLite tidak bisa ditulis" "$f"; fi
done

mkdir -p "$CMS_ASSETS" 2>/dev/null || fail "folder gambar tidak bisa dibuat" "$CMS_ASSETS"
[ -w "$CMS_ASSETS" ] || fail "folder gambar tidak bisa ditulis" "$CMS_ASSETS"
# Gambar contoh dipakai halaman bawaan; salin bila volume masih kosong.
[ -f "$CMS_ASSETS/placeholder.svg" ] || cp /app/defaults/placeholder.svg "$CMS_ASSETS/placeholder.svg"

exec /app/DCMS
