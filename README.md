# DCMS

CMS dan situs pribadi yang ditulis sepenuhnya dengan **Haskell**: server WAI + Warp, database SQLite,
HTML dari Lucid, dan konten Markdown. Satu program, satu file database, tanpa framework web besar.

## Fitur

- **Page builder**: susun halaman dari blok (hero, kartu, harga, FAQ, galeri, slider, kode custom, dll.)
  dan edit langsung di halaman.
- **Blog**: editor Markdown dengan pratinjau, kategori, alur review (User → Editor/Admin), dan skor SEO.
- **SEO & analitik**: sitemap, robots.txt, meta Open Graph, kunjungan per sumber tanpa cookie.
- **API Gateway**: REST API JSON di `/api/v1` (artikel, kategori, info situs) dengan API key, kuota, dan CORS.
  Dokumentasi di `/api/docs`.
- **Log keamanan**: login gagal/diblokir, pemindaian celah, akses ditolak, perubahan akun, dan lainnya.
  Bisa diunduh sebagai laporan **Excel** atau **PDF** bernomor dengan sidik SHA-256.
- **Tampilan hidup**: judul h1 di semua halaman jatuh per huruf lalu bergelombang, h2 teracak lalu
  terbaca. Home punya efek tambahan (orbit ikon, pita berjalan, kartu 3D). Semua gerak otomatis mati bila
  pengunjung memilih gerak minimal.
- **Cepat**: font Nunito & ikon disajikan sendiri tanpa CDN (ikon hanya subset yang dipakai, ~22 KB),
  aset berversi di-cache permanen, dan halaman dikirim dengan gzip.
- **Peran pengguna**:

  | Peran | Hak akses |
  |---|---|
  | Administrator | Semua, termasuk pengguna, pengaturan, API, dan log |
  | Editor | Semua artikel, kategori, halaman, dan SEO |
  | User | Artikel miliknya sendiri (draf atau kirim untuk review) |

## Menjalankan

Butuh **GHC 9.10.3** dan cabal (mis. lewat [ghcup](https://www.haskell.org/ghcup/)).

```bash
cabal run DCMS
```

Buka <http://localhost:3000> dan login di `/login` dengan `admin` / `admin12345`. Segera ganti password di
menu **Profil**. Lupa password: `cabal run DCMS -- reset-password admin`.

> Isi folder `static` dimuat ke memori saat start, jadi restart server setelah mengubahnya.
> Menambah ikon Font Awesome baru di kode? Jalankan `python3 tools/fonts.py` (butuh `pip install fonttools brotli`)
> agar ikon itu masuk subset. Ikon di luar subset tetap tampil lewat CDN sebagai cadangan.

### Pengaturan (variabel lingkungan)

| Variabel | Bawaan | Keterangan |
|---|---|---|
| `CMS_PORT` | `3000` | Port HTTP |
| `CMS_DB` | `cms.db` | File database SQLite |
| `CMS_STATIC` | `static` | Folder CSS/JS |
| `CMS_ASSETS` | `uye/asset/img` | Folder gambar (`/asset/img/`) |
| `CMS_ADMIN_PASSWORD` | `admin12345` | Password admin saat database pertama dibuat |
| `CMS_SITE_URL` | — | Alamat resmi, mis. `https://contoh.id` (HTTPS → cookie Secure + HSTS) |
| `CMS_TRUST_PROXY` | — | Isi `1` bila berjalan di balik reverse proxy |

## Arsitektur

Alur satu request: **App → Handler → DB → View**.

- **App** menerima request, membaca sesi & form, memeriksa CSRF, lalu meneruskan ke rute yang cocok.
  Setelah itu header keamanan ditambahkan dan kunjungan/aktivitas mencurigakan dicatat.
- **Handler** berisi logika tiap halaman: publik, dashboard admin, dan API.
- **DB** memegang skema, migrasi otomatis saat start, dan semua query SQLite.
- **View** menghasilkan HTML dengan Lucid. Tidak ada template terpisah; semuanya fungsi Haskell.
- **Modul pendukung**: autentikasi, batas percobaan login, page builder, SEO, analitik, log keamanan,
  serta penulis Excel/PDF/ZIP sendiri (tanpa library tambahan).

```
app/Main.hs          konfigurasi & start server
uye/CMS/App.hs       middleware & routing
uye/CMS/Handlers/    logika: Public, Admin, Api
uye/CMS/Views/       HTML: situs, dashboard, dokumentasi API
uye/CMS/DB.hs        skema & query SQLite
uye/CMS/*.hs         modul pendukung (Auth, Blocks, Seo, Log, Pdf, Xlsx, …)
static/              CSS, JS, font (home.css/home.js hanya untuk Home)
tools/fonts.py       pembuat ulang font & subset ikon di static/
deploy/              contoh Caddy, Podman Quadlet, entrypoint Docker
```

## Keamanan

Password bcrypt, token sesi acak (disimpan sebagai hash), cookie `HttpOnly` + `SameSite`, token CSRF di
semua form, batas percobaan login, unggahan gambar dicek dari isi berkas, HTML Markdown dibersihkan,
blok kode custom berjalan di iframe sandbox, dan header keamanan (CSP, HSTS, dll.) di setiap respons.

## Deploy

Butuh server yang bisa menjalankan program sendiri dengan disk tetap (VPS atau cloud VM). Hosting PHP biasa
dan platform tanpa disk tetap tidak cocok karena data ada di file SQLite.

**Dengan Docker** (paling mudah):

```bash
cp .env.example .env                        # atur password & domain
docker compose up -d --build                # lokal: http://localhost:8080
docker compose --profile https up -d --build  # produksi dengan HTTPS (Caddy)
```

**Tanpa Docker**: build di server dengan `cabal build exe:DCMS`, jalankan binary-nya lewat systemd, dan pasang
Caddy sebagai reverse proxy HTTPS (contoh di `deploy/Caddyfile`). Jangan buka port 3000 ke internet.

**Memindahkan isi situs**: salin `cms.db` (beserta `-wal`/`-shm` bila ada) dan folder `uye/asset/img` ke server,
lalu atur `CMS_SITE_URL`. Untuk backup, salin dua hal yang sama secara berkala.
