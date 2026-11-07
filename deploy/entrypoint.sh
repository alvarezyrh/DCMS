#!/bin/sh
# Siapkan folder data (/data) lalu jalankan DCMS sebagai user "dcms" (uid 10001).
#
# Bila kontainer mulai sebagai root (bawaan Docker & Podman): rapikan pemilik /data
# (mis. cms.db yang disalin sebagai root, atau folder host), lalu turunkan hak dengan
# setpriv. Bila sudah dijalankan sebagai user lain (--user), langsung cek izin tulis.
set -eu

APP_UID=10001
APP_GID=10001
DATA_DIR=$(dirname "$CMS_DB")

fail() {
  echo "GALAT: $1" >&2
  echo "  Aplikasi berjalan sebagai uid $(id -u), tetapi $2 tidak bisa ditulis." >&2
  echo "  Perbaikan: jalankan kontainer tanpa --user (biar entrypoint merapikan izinnya)," >&2
  echo "  atau di host: sudo chown -R $APP_UID:$APP_GID <folder-data>" >&2
  exit 1
}

if [ "$(id -u)" = "0" ]; then
  mkdir -p "$DATA_DIR" "$CMS_ASSETS"
  # Hanya chown bila perlu (volume besar tidak diproses ulang tiap start).
  if [ -n "$(find "$DATA_DIR" "$CMS_ASSETS" \( ! -user $APP_UID -o ! -group $APP_GID \) -print -quit 2>/dev/null)" ]; then
    echo "[entrypoint] Menyesuaikan pemilik $DATA_DIR ke uid $APP_UID…"
    chown -R $APP_UID:$APP_GID "$DATA_DIR" "$CMS_ASSETS" || fail "gagal mengubah pemilik folder data" "$DATA_DIR"
  fi
  [ -f "$CMS_ASSETS/placeholder.svg" ] || install -o $APP_UID -g $APP_GID -m 644 /app/defaults/placeholder.svg "$CMS_ASSETS/placeholder.svg"
  exec setpriv --reuid=$APP_UID --regid=$APP_GID --clear-groups --inh-caps=-all --bounding-set=-all /app/DCMS
fi

# Sudah bukan root: cek izin tulis agar error-nya jelas (bukan "readonly database" dari SQLite).
touch "$DATA_DIR/.write-test" 2>/dev/null && rm -f "$DATA_DIR/.write-test" \
  || fail "folder data tidak bisa ditulis" "$DATA_DIR"
for f in "$CMS_DB" "$CMS_DB-wal" "$CMS_DB-shm"; do
  if [ -e "$f" ] && [ ! -w "$f" ]; then fail "berkas database tidak bisa ditulis (readonly database)" "$f"; fi
done
mkdir -p "$CMS_ASSETS" 2>/dev/null || fail "folder gambar tidak bisa dibuat" "$CMS_ASSETS"
[ -w "$CMS_ASSETS" ] || fail "folder gambar tidak bisa ditulis" "$CMS_ASSETS"
[ -f "$CMS_ASSETS/placeholder.svg" ] || cp /app/defaults/placeholder.svg "$CMS_ASSETS/placeholder.svg"
exec /app/DCMS
