# DCMS

CMS ini dibangun menggunakan Haskell: server **WAI + Warp** (tanpa framework web besar),
database **SQLite**, HTML dari **Lucid**, password **bcrypt**, dan konten **Markdown**.

## Fitur

- **Situs publik**: beranda, halaman artikel, arsip kategori, pencarian, paginasi.
- **Login & registrasi** dengan sesi berbasis cookie (token di-hash SHA-256 di database).
- **Dashboard**: statistik, daftar artikel menunggu review, artikel terakhir diperbarui.
- **Editor artikel**: toolbar Markdown, pratinjau, hitung kata, Ctrl+S, slug otomatis.
- **CRUD**: artikel, kategori, pengguna, profil, pengaturan situs.
- **Page builder**: edit halaman langsung di tempat, plus panel **Tampilan situs** untuk mengatur ukuran website
  (lebar konten & jarak tepi) serta warna, jarak, dan bentuk navigasi atas & bawah (berlaku di seluruh situs,
  tersimpan bersama tombol Simpan).
- **Kartu fitur**: 13 gaya (kartu, garis tepi, melayang, ikon di samping, tengah, minimalis, aksen atas/samping,
  bernomor, ikon penuh, gradasi, kaca, gelap), 2–4 kolom, bentuk sudut, dan bentuk ikon lewat chip "Gaya kartu".
- **Blok siap pakai**: testimoni (rating bintang), FAQ (dengan data terstruktur Google), paket harga (paket
  unggulan disorot), tim, galeri, logo klien, dan video YouTube.
- **Gaya situs**: warna utama, 8 pilihan huruf, animasi muncul saat digulir, dan tombol kembali ke atas
  (panel Tampilan situs di editor).
- **SEO & kontak**: deskripsi situs untuk Google dan pratinjau tautan (Open Graph), favicon otomatis, tombol
  WhatsApp melayang, dan ikon media sosial di footer (menu Pengaturan).
- **SEO artikel**: kata kunci utama, judul SEO, deskripsi meta, gambar pratinjau, dan noindex per artikel, dengan
  pratinjau hasil Google dan skor SEO langsung (12 pemeriksaan) saat menulis. Halaman artikel otomatis memuat
  meta Open Graph, canonical, dan data terstruktur BlogPosting.
- **Menu SEO & Analitik** (Admin & Editor): kunjungan harian per sumber (mesin pencari, media sosial, langsung,
  situs lain) tanpa cookie, halaman terpopuler, audit skor SEO semua artikel, serta integrasi Google Search
  Console, Bing, Google Analytics 4, `sitemap.xml`, dan `robots.txt` otomatis. Ringkasannya tampil sebagai
  widget di dashboard.
- **Popup gambar**: gambar di halaman dan artikel bisa diklik untuk dibuka besar (tombol panah, Esc untuk
  menutup). Bisa dimatikan di panel Tampilan situs.
- **Blok Kode custom**: HTML, CSS, dan JavaScript sendiri di page builder. Dijalankan di iframe sandbox
  (terpisah dari situs), jadi skripnya tidak bisa membaca sesi login atau memakai hak akses admin.
- **Keamanan**: proteksi CSRF di semua form POST, cookie HttpOnly + SameSite, HTML mentah di Markdown dibuang,
  cegah open redirect, header keamanan, user dinonaktifkan langsung keluar.

## Peran

| Peran | Hak akses |
|---|---|
| **Administrator** | Semua hal: artikel, kategori, pengguna, pengaturan situs |
| **Editor** | Semua artikel (termasuk terbitkan), kategori |
| **User** | Artikel miliknya sendiri; hanya bisa Draf atau "Menunggu review" |

Akun hasil registrasi publik otomatis menjadi **User**. Admin tidak bisa menghapus, menonaktifkan,
atau menurunkan peran akunnya sendiri.

## Menjalankan

Butuh GHC ≥ 9.4 dan cabal (mis. lewat [ghcup](https://www.haskell.org/ghcup/)).

```bash
cabal run DCMS
```

Buka <http://localhost:3000>. Saat database pertama kali dibuat, akun admin dibuat otomatis:

- username: `admin`
- password: `admin12345` (atau nilai `CMS_ADMIN_PASSWORD`)

Segera ganti password lewat **Profil**.

### Variabel lingkungan

| Variabel | Default | Keterangan |
|---|---|---|
| `CMS_PORT` | `3000` | Port HTTP |
| `CMS_DB` | `cms.db` | Lokasi file SQLite |
| `CMS_STATIC` | `static` | Folder CSS/JS |
| `CMS_ASSETS` | `src/asset/img` | Folder gambar (foto profil, gambar halaman), disajikan di `/asset/img/` |
| `CMS_ADMIN_PASSWORD` | `admin12345` di lokal, acak di HTTPS | Password admin awal (hanya saat seed, tidak dicetak ke log) |
| `CMS_SITE_URL`, `CMS_TRUST_PROXY` | — | Untuk produksi, lihat di bawah |

> Fedora tanpa `gmp-devel`: buat symlink `ln -s /usr/lib64/libgmp.so.10 ~/.local/lib/libgmp.so`
> lalu build dengan `LIBRARY_PATH=~/.local/lib cabal build`.

## Struktur

```
app/Main.hs               konfigurasi & start server
src/CMS/Types.hs          tipe domain, peran, izin
src/CMS/DB.hs             skema & query SQLite
src/CMS/Auth.hs           bcrypt, token acak
src/CMS/Web.hs            parameter, respons, cookie, flash
src/CMS/App.hs            middleware (sesi, CSRF), routing, seed
src/CMS/Handlers/*.hs     handler publik & admin
src/CMS/Views/*.hs        HTML (Lucid)
static/style.css, app.js  tampilan & editor
```

## Rute

| Metode | Path | Akses |
|---|---|---|
| GET | `/`, `/post/:slug`, `/category/:slug` | publik |
| GET/POST | `/login`, `/register`; POST `/logout` | publik |
| GET | `/admin` | login |
| GET/POST | `/admin/posts`, `/admin/posts/new`, `/admin/posts/:id/edit`, `/admin/posts/:id`, `/admin/posts/:id/delete` | login (User: milik sendiri) |
| POST | `/admin/posts/:id/publish` | admin, editor |
| GET/POST | `/admin/categories…` | admin, editor |
| GET/POST | `/admin/users…`, `/admin/settings` | admin |
| GET/POST | `/admin/profile`, `/admin/profile/password` | login |

## Untuk produksi

Semua pengaturan produksi lewat variabel lingkungan:

| Variabel | Fungsi |
|---|---|
| `CMS_SITE_URL` | Alamat resmi situs, mis. `https://contoh.id`. Dipakai untuk canonical, sitemap, dan Open Graph (kebal header Host palsu). Bila diawali `https://`, cookie otomatis diberi `Secure` dan HSTS aktif. |
| `CMS_TRUST_PROXY=1` | Wajib bila berjalan di balik reverse proxy: IP pengunjung diambil dari `X-Forwarded-For` (untuk batas percobaan login). Jangan aktifkan tanpa proxy. |
| `CMS_ADMIN_PASSWORD` | Password akun `admin` saat database pertama kali dibuat. Tidak dicetak ke log. Bila kosong di produksi, dibuat acak dan ditampilkan sekali di log. |
| `CMS_DB`, `CMS_ASSETS`, `CMS_STATIC`, `CMS_PORT` | Lokasi database, folder gambar, folder statis, dan port. |
| `CMS_COOKIE_SECURE=0/1` | Paksa atribut `Secure` cookie (biasanya tidak perlu, ikut `CMS_SITE_URL`). |

### Keamanan bawaan

- Password bcrypt (cost 11); token sesi acak 256-bit, disimpan sebagai hash SHA-256; sesi dihapus saat
  logout, ganti password, atau perubahan peran.
- Cookie `HttpOnly` + `SameSite=Lax` (+ `Secure` di HTTPS); token CSRF di semua form POST.
- **Batas percobaan login**: 5 salah per IP+akun, 10 per akun, 30 per IP dalam 15 menit → dikunci sementara (429).
- Markdown membuang HTML mentah dan tautan `javascript:`; semua teks di-escape oleh Lucid.
- Unggahan dicek dari isi berkas (JPG/PNG/WebP/GIF, maks. 2 MB), nama acak, SVG ditolak; folder aset
  disajikan dengan CSP `sandbox`.
- Blok "Kode custom" berjalan di iframe sandbox (origin terpisah): skripnya tidak bisa membaca sesi.
- Header: `X-Frame-Options`, `X-Content-Type-Options`, `Referrer-Policy`, `Content-Security-Policy`
  (object/base/frame-ancestors), `Permissions-Policy`, `Cross-Origin-Opener-Policy`, dan HSTS di HTTPS.
- Pembatasan peran diperiksa di server (User tidak bisa menerbitkan, mengedit milik orang lain, atau membuka menu staf).

## Deploy dengan Podman di AWS

Rekomendasi: **satu instans EC2** (Amazon Linux 2023 atau Ubuntu 24.04) dengan disk EBS. Database SQLite
butuh disk lokal yang awet, jadi ECS Fargate/App Runner tanpa disk tetap **tidak** cocok.

Berkas yang disediakan:

- `Containerfile` — image dua tahap (build Haskell → runtime Debian slim, user non-root, healthcheck).
- `deploy/compose.yaml` — DCMS + Caddy (HTTPS otomatis Let's Encrypt).
- `deploy/quadlet/` — unit systemd (Podman Quadlet): jalan otomatis saat boot dan restart bila crash.
- `deploy/Caddyfile`, `deploy/.env.example`.

**1. Siapkan EC2**

- Tipe: `t3.small`/`t4g.small` atau lebih (build GHC butuh ±3 GB RAM; tambahkan swap 2–4 GB bila RAM 2 GB).
- Disk: EBS gp3 20 GB. Pasang Elastic IP, lalu arahkan DNS domain (A record) ke IP tersebut.
- Security group: buka **80** dan **443** untuk semua, **22** hanya untuk IP Anda. Port 3000 **jangan** dibuka.

**2. Pasang Podman & build image**

```bash
sudo dnf install -y podman git          # Ubuntu: sudo apt install -y podman git
git clone <repo-anda> dcms && cd dcms   # atau salin folder proyek dengan scp/rsync
sudo podman build --format docker -t localhost/dcms:latest .
```

**3. Konfigurasi & jalankan (Quadlet/systemd)**

```bash
sudo mkdir -p /etc/dcms
sudo cp deploy/Caddyfile /etc/dcms/Caddyfile
sudo tee /etc/dcms/dcms.env >/dev/null <<'ENV'
DCMS_DOMAIN=contoh.id
CMS_SITE_URL=https://contoh.id
CMS_ADMIN_PASSWORD=ganti-dengan-password-kuat
ENV
sudo chmod 600 /etc/dcms/dcms.env
sudo cp deploy/quadlet/* /etc/containers/systemd/
sudo systemctl daemon-reload
sudo systemctl start dcms caddy
sudo podman logs dcms                   # cek "[config] … cookie Secure: True"
```

Buka `https://contoh.id/admin`, login sebagai `admin`, lalu **segera ganti password** di menu Profil.
Di menu **SEO & Analitik → Integrasi**, kirim `https://contoh.id/sitemap.xml` ke Google Search Console.

Alternatif tanpa systemd: `cd deploy && cp .env.example .env` (isi), lalu `podman compose up -d`.

**Memindahkan data lama (opsional)** — salin `cms.db` dan folder gambar ke volume, lalu nyalakan:

```bash
sudo systemctl stop dcms
sudo podman volume create dcms-data
sudo cp cms.db "$(sudo podman volume inspect dcms-data --format '{{.Mountpoint}}')/cms.db"
sudo cp -r src/asset/img "$(sudo podman volume inspect dcms-data --format '{{.Mountpoint}}')/assets"
sudo systemctl start dcms     # opsi :U di unit otomatis menyesuaikan pemilik berkas
```

**4. Update aplikasi**

```bash
git pull && sudo podman build --format docker -t localhost/dcms:latest . && sudo systemctl restart dcms
```

**5. Backup**

Data ada di volume `dcms-data` (database + gambar unggahan). Cara termudah: jadwalkan **snapshot EBS**
harian lewat AWS Backup. Untuk salinan manual:

```bash
sudo systemctl stop dcms && sudo podman volume export dcms-data > dcms-$(date +%F).tar && sudo systemctl start dcms
```

**Pemecahan masalah: "readonly" / "Permission denied"**

Aplikasi di dalam kontainer berjalan sebagai user `dcms` (uid 10001), bukan root. Bila isi volume dimiliki
user lain (mis. `cms.db` disalin sebagai root, atau memakai folder host), muncul `attempt to write a readonly
database` atau `Permission denied`, dan log kontainer menampilkan `GALAT: …` beserta cara memperbaikinya.

- Pasang volume dengan opsi **`:U`** (sudah ada di `deploy/quadlet/dcms.container` dan `deploy/compose.yaml`):
  `-v dcms-data:/data:U`. Untuk folder host: `-v /srv/dcms:/data:U,Z` (`Z` untuk SELinux).
- Atau ubah pemilik di host: `sudo chown -R 10001:10001 <folder-data>`.
- Pesan `Read-only file system` saat menulis ke luar `/data` itu disengaja (`ReadOnly=true`); semua data
  aplikasi memang disimpan di `/data`.
