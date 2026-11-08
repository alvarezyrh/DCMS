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
- **Halaman baru** (menu Halaman, Admin & Editor): buat halaman sendiri di `/alamat-halaman` dari 9 templat
  (kosong, standar, landing page, tentang, layanan, harga, FAQ, galeri & video, kontak) atau salin isi halaman
  lain, lalu langsung diedit di page builder. Tiap halaman punya status **Terbit/Draf** (draf hanya terlihat staf,
  otomatis noindex), pilihan **tampil di menu** (atas, bawah, footer) dengan ikon menu, urutan menu (naik/turun),
  SEO sendiri (judul, deskripsi, gambar pratinjau, noindex + pratinjau Google), duplikat, ubah alamat, dan hapus.
  Halaman bawaan tidak bisa dihapus dan alamatnya tetap. Halaman terbit otomatis masuk `sitemap.xml`.
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
- **Menu Cabang** (Admin & Editor): kelola cabang/outlet (alamat, telepon & WhatsApp, jam, catatan, tautan peta,
  urutan, aktif/nonaktif). Halaman `/branch` menampilkannya lewat blok **Direktori cabang**: dikelompokkan per
  provinsi, dengan pencarian, filter provinsi (bisa ditautkan: `/branch?provinsi=Bali`, `/branch?cari=surabaya`),
  tombol WhatsApp/peta/rute, dan data terstruktur Store untuk Google. Berisi awal 122 outlet.
  Setiap cabang bernomor urut; tombol **Urutkan dari terdekat** memakai lokasi pengunjung (izin browser,
  butuh HTTPS) untuk menomori cabang dari yang terdekat sampai terjauh, lengkap dengan jaraknya.
  Koordinat cabang diatur di form Cabang (atau diambil otomatis dari tautan Google Maps lengkap).
- **API & API Gateway** (menu khusus Admin): REST API JSON di `/api/v1` untuk web/aplikasi lain — Cabang
  (`/branches`, filter `q`/`province`/`city`, urutan terdekat `near=lat,lng` + `radius_km`, detail, per provinsi),
  Artikel, Kategori, dan Info situs. Tiap API bisa diaktifkan/dimatikan dan diatur aksesnya (publik atau wajib
  API key). API key disimpan sebagai hash, punya cakupan API dan batas permintaan per menit; pemakaian dicatat
  per API dan per key; CORS diatur per domain. Dokumentasi di `/api/docs`, spesifikasi di `/api/openapi.json`.
  Contoh: `curl -H "X-API-Key: dcms_…" https://domain-anda/api/v1/branches?near=-6.2,106.8&per_page=5`
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

Lupa password? Jalankan `cabal run DCMS -- reset-password admin` (atau di Docker:
`docker compose exec -u 10001 dcms /app/DCMS reset-password admin`). Semua sesi pengguna itu ikut dikeluarkan.

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
app/Main.hs                  konfigurasi dari env, start server Warp, perintah reset-password
src/CMS/Types.hs             tipe domain (User, Post, Branch, Settings, Ctx…), peran & izin
src/CMS/DB.hs                skema, migrasi, seed, dan semua query SQLite
src/CMS/App.hs               middleware (body, sesi, CSRF, header keamanan), routing, analitik, seed
src/CMS/Web.hs               parameter form/query, respons, cookie, flash, redirect
src/CMS/Auth.hs              bcrypt, token acak, hash token
src/CMS/RateLimit.hs         batas percobaan login (per IP, akun, IP+akun)
src/CMS/Blocks.hs            page builder: jenis blok, validasi, templat & halaman bawaan
src/CMS/NavStyle.hs          Tampilan situs: Tema, Navigasi, Footer → variabel CSS
src/CMS/Seo.hs               analisis & skor SEO artikel
src/CMS/Analytics.hs         klasifikasi sumber kunjungan, deteksi bot
src/CMS/Api.hs               katalog & konfigurasi API Gateway
src/CMS/Upload.hs            unggah gambar (cek isi berkas) & sajikan /asset/img
src/CMS/Status.hs            status server (CPU, RAM, disk, uptime) untuk dashboard
src/CMS/BranchSeed.hs        data awal cabang
src/CMS/Handlers/Public.hs   situs publik, login, register, sitemap, robots
src/CMS/Handlers/Admin.hs    dashboard (semua menu /admin)
src/CMS/Handlers/Api.hs      REST API /api/v1 + gateway (key, kuota, CORS)
src/CMS/Views/Layout.hs      kerangka HTML situs & dashboard, header, footer, meta SEO
src/CMS/Views/Site.hs        render blok halaman + mode edit (panel builder)
src/CMS/Views/Public.hs      artikel, kategori, login, register
src/CMS/Views/Admin.hs       dashboard, artikel, kategori, pengguna, profil, pengaturan
src/CMS/Views/PageAdmin.hs   menu Halaman: daftar, buat baru, pengaturan
src/CMS/Views/BranchAdmin.hs menu Cabang
src/CMS/Views/SeoAdmin.hs    menu SEO & Analitik
src/CMS/Views/ApiAdmin.hs    menu API Gateway
src/CMS/Views/ApiDocs.hs     /api/docs & /api/openapi.json
static/style.css             tampilan situs & dashboard
static/app.js                interaksi umum (menu HP, lightbox, popup, form, SEO langsung, dashboard)
static/builder.js            page builder (edit di tempat, seret blok, panel Tampilan situs)
```

## Arsitektur

Diagram di bawah memakai [Mermaid](https://mermaid.js.org) dan tampil otomatis di GitHub/GitLab.

### 1. Gambaran sistem

```mermaid
flowchart LR
    subgraph Klien["Klien"]
        V["Pengunjung<br/>(browser / HP)"]
        S["Admin, Editor, User<br/>(browser)"]
        X["Aplikasi lain<br/>(web / mobile / server)"]
    end

    subgraph Server["Server (Docker / Podman)"]
        C["Caddy<br/>HTTPS otomatis (Let's Encrypt)<br/>profil https, opsional"]
        subgraph App["Kontainer DCMS (Haskell)"]
            W["Warp + WAI<br/>port 3000"]
            M["CMS.App<br/>middleware & routing"]
            H["Handlers<br/>Public · Admin · Api"]
            VW["Views (Lucid)<br/>HTML di server"]
        end
        subgraph Data["Volume /data"]
            DB[("SQLite<br/>cms.db (WAL)")]
            AS[("Folder aset<br/>/data/assets<br/>gambar unggahan")]
        end
        ST["/app/static<br/>style.css · app.js · builder.js"]
    end

    subgraph Luar["Layanan luar (di browser)"]
        GF["Google Fonts"]
        FA["Font Awesome (cdnjs)"]
        YT["YouTube embed"]
        GA["Google Analytics 4<br/>(opsional)"]
        GM["Google Maps"]
    end

    V -- "HTTPS" --> C
    S -- "HTTPS" --> C
    X -- "HTTPS + X-API-Key" --> C
    C -- "HTTP, X-Forwarded-For" --> W
    W --> M --> H --> VW
    H <--> DB
    H <--> AS
    W --> ST
    V -.-> GF & FA & YT & GA & GM
```

Tanpa Caddy (lokal), Warp diakses langsung di `127.0.0.1:8080`. Satu proses melayani situs, dashboard, dan API. Koneksi SQLite dibungkus `MVar`, jadi aman dipakai banyak thread Warp.

### 2. Lapisan modul

```mermaid
flowchart TD
    Main["app/Main.hs<br/>baca env · openDb · seed · runSettings"] --> App

    subgraph L1["Lapisan HTTP"]
        App["CMS.App<br/>mkApp · handle · route · securityHeaders · trackView"]
        Web["CMS.Web<br/>param · respons · cookie · flash"]
    end

    subgraph L2["Handler"]
        HP["Handlers.Public<br/>beranda · halaman · blog · artikel · kategori<br/>login · register · sitemap · robots"]
        HA["Handlers.Admin<br/>dashboard · halaman · artikel · kategori · cabang<br/>SEO · API · pengguna · profil · pengaturan · unggah"]
        HX["Handlers.Api<br/>gateway · endpoint JSON · CORS"]
    end

    subgraph L3["Tampilan (Lucid → HTML)"]
        VL["Views.Layout<br/>header · menu · footer · meta SEO · sidebar admin"]
        VS["Views.Site<br/>render blok · mode edit · panel builder"]
        VP["Views.Public"]
        VA["Views.Admin · PageAdmin · BranchAdmin<br/>SeoAdmin · ApiAdmin · ApiDocs"]
    end

    subgraph L4["Logika domain"]
        BL["CMS.Blocks<br/>jenis blok · sanitasi · templat"]
        NS["CMS.NavStyle<br/>Tema · Navigasi · Footer → CSS"]
        SEO["CMS.Seo"]
        AN["CMS.Analytics"]
        API["CMS.Api"]
        AU["CMS.Auth · CMS.RateLimit"]
        UP["CMS.Upload"]
        STS["CMS.Status"]
        UT["CMS.Util · CMS.Types"]
    end

    DBM["CMS.DB<br/>skema · migrasi · query"] --> SQL[("SQLite")]

    App --> HP & HA & HX
    App --> Web
    HP --> VP & VS
    HA --> VA & VS
    HX --> API
    VS & VP & VA --> VL
    HP & HA & HX --> DBM
    HA --> BL & NS & SEO & UP & STS
    HP --> AU
    App --> AN
    VS --> BL & NS
    VL --> NS
```

### 3. Siklus satu request

```mermaid
flowchart TD
    R(["Request masuk ke Warp"]) --> ST{"GET /static/:file?"}
    ST -- ya --> STF["Kirim style.css / app.js / builder.js<br/>Cache-Control: no-cache"] --> Z(["Selesai"])
    ST -- tidak --> AS{"GET /asset/img/:file?"}
    AS -- ya --> ASF["Kirim gambar unggahan<br/>CSP sandbox, nama divalidasi"] --> Z
    AS -- tidak --> P{"Metode POST?"}
    P -- ya --> PB["Parse body multipart/form<br/>maks. 1 berkas 2 MB, isian 2 MB"]
    PB --> TB{"Terlalu besar?"}
    TB -- ya --> E413["413 Berkas terlalu besar"]
    P -- tidak --> LD
    TB -- tidak --> LD["Baca cookie: sid, csrf, flash"]
    LD --> DBQ["Satu akses DB:<br/>user dari sesi (hash SHA-256 token)<br/>+ pengaturan situs + menu halaman"]
    DBQ --> CTX["Bangun Ctx:<br/>user, form, query, berkas, csrf,<br/>flash, settings, menu, tahun"]
    CTX --> CS{"POST dan _csrf<br/>≠ cookie csrf?"}
    CS -- ya --> E403["403 Formulir kedaluwarsa"]
    CS -- tidak --> RT["route: pilih handler<br/>(lihat diagram 4)"]
    RT --> EX{"Exception?"}
    EX -- ya --> E500["500 + log stderr"]
    EX -- tidak --> RS["Respons handler"]
    E413 & E403 & E500 & RS --> TV{"Hitung kunjungan?<br/>GET · 200 · HTML · halaman publik<br/>· tidak login · bukan ?edit=1 · bukan bot"}
    TV -- ya --> REC["page_views +1<br/>(hari, path, sumber, mesin pencari)"]
    TV -- tidak --> CK
    REC --> CK["Atur cookie:<br/>csrf baru bila belum ada · hapus flash terpakai<br/>· hapus sid tidak valid"]
    CK --> SH["Header keamanan:<br/>X-Frame-Options, nosniff, Referrer-Policy, CSP,<br/>Permissions-Policy, COOP, HSTS + Secure (HTTPS)"]
    SH --> Z
```

### 4. Routing

```mermaid
flowchart TD
    RT{"route<br/>(metode, path)"}
    RT -- "GET /" --> HOME["Public.home<br/>?q= → /blog?q=…<br/>selain itu halaman builder 'home'"]
    RT -- "/api/…" --> API["Api.apiRoute<br/>(diagram 11)"]
    RT -- "OPTIONS /api/…" --> PRE["Api.apiPreflight<br/>CORS 204"]
    RT -- "GET /sitemap.xml" --> SM["sitemap: halaman terbit,<br/>artikel, kategori (tanpa noindex)"]
    RT -- "GET /robots.txt" --> RB["robots: tutup /admin, /login, ?edit=1<br/>atau seluruh situs bila noindex"]
    RT -- "GET /post/:slug" --> PO["artikel + meta OG + JSON-LD"]
    RT -- "GET /category/:slug" --> CA["arsip kategori + paginasi"]
    RT -- "GET/POST /login, /register<br/>POST /logout" --> AUTH["autentikasi<br/>(diagram 6)"]
    RT -- "GET /blog" --> BLOG["blok halaman 'blog'<br/>+ daftar artikel, cari, paginasi"]
    RT -- "GET /home" --> RH["303 → /"]
    RT -- "* /admin/…" --> AD{"Sudah login?"}
    AD -- tidak --> LG["303 → /login?next=…"]
    AD -- ya --> ADM["Admin.adminRoute<br/>(diagram 7)"]
    RT -- "GET /:slug<br/>(bukan alamat sistem)" --> PG["Public.sitePage<br/>(diagram 5)"]
    RT -- "lainnya" --> NF["404 Tidak ditemukan"]
```

Alamat sistem (`admin`, `api`, `static`, `asset`, `post`, `category`, `login`, `register`, `logout`, `blog`, `home`,
`sitemap.xml`, `robots.txt`, `new`, dll.) tidak pernah dianggap halaman, dan tidak bisa dipakai sebagai alamat
halaman baru.

### 5. Website: menampilkan halaman

```mermaid
flowchart TD
    A(["GET /:slug atau /"]) --> F["findPage slug<br/>(tabel pages)"]
    F --> EX{"Ada?"}
    EX -- tidak --> N404["404"]
    EX -- ya --> VIS{"Terbit, atau<br/>pengunjung Admin/Editor?"}
    VIS -- tidak --> N404
    VIS -- ya --> ED{"?edit=1 dan Admin/Editor?"}
    ED -- ya --> E1["Mode edit:<br/>ambil 6 artikel + semua cabang aktif"]
    ED -- tidak --> E2["Mode lihat:<br/>ambil artikel sebanyak blok 'Artikel terbaru' perlu<br/>+ cabang bila ada blok 'Direktori cabang'"]
    E1 & E2 --> PD["PageData: artikel, cabang, nama situs"]
    PD --> LAY["siteLayoutMeta"]

    subgraph HEAD["&lt;head&gt;"]
        M1["title & deskripsi: SEO halaman → bawaan situs"]
        M2["canonical, Open Graph, Twitter card,<br/>noindex bila draf / noindex / situs ditutup"]
        M3["favicon, verifikasi Google/Bing, GA4"]
        M4["huruf Tema (Google Fonts)"]
        M5["&lt;style id=nav-style&gt;<br/>variabel CSS dari NavStyle:<br/>warna utama & latar, kotak halaman, lebar,<br/>logo, navigasi atas/bawah, footer"]
    end

    subgraph BODY["&lt;body data-lightbox data-reveal data-page-box&gt;"]
        B1["Header: logo website + menu (cMenu)<br/>+ tombol Masuk/Dashboard + ☰ di HP"]
        B2["Bar editor + panel blok & Tampilan situs<br/>(hanya mode edit)"]
        B3["Banner draf (hanya staf, bukan mode edit)"]
        B4["main#pb-canvas: renderBlock untuk tiap blok"]
        B5["Footer: logo, teks, menu, sosial, hak cipta {tahun}"]
        B6["Tombol WhatsApp & kembali ke atas"]
        B7["Menu bawah (6 halaman pertama)"]
    end

    LAY --> HEAD
    LAY --> BODY
    B4 --> RB{"Jenis blok"}
    RB --> K1["hero · head · text · split · stats · cta · image"]
    RB --> K2["features (13 gaya kartu) · testimonials · faq (+JSON-LD)<br/>pricing · team · gallery · logos"]
    RB --> K3["posts (artikel terbaru) · branches · branchdir<br/>(direktori cabang + Store JSON-LD)"]
    RB --> K4["slider · video (YouTube) · code (iframe sandbox)<br/>· popup (dialog brosur)"]
    BODY --> JS["static/app.js di browser:<br/>menu HP, animasi muncul, slider, lightbox,<br/>popup brosur (jeda & frekuensi), urut cabang terdekat,<br/>tinggi iframe kode"]
```

### 6. Login, registrasi, dan sesi

```mermaid
flowchart TD
    L(["POST /login<br/>login + password + next"]) --> RL{"Diblokir RateLimit?<br/>5 salah per IP+akun, 10 per akun,<br/>30 per IP / 15 menit"}
    RL -- ya --> T429["429 coba lagi dalam N menit"]
    RL -- tidak --> FU["Cari user (username atau email)"]
    FU --> FX{"Ada?"}
    FX -- tidak --> DUM["bcrypt palsu (waktu respons sama)"] --> BAD["401 + catat gagal"]
    FX -- ya --> PW{"Password cocok? (bcrypt)"}
    PW -- tidak --> BAD
    PW -- ya --> AC{"Akun aktif?"}
    AC -- tidak --> OFF["401 akun dinonaktifkan"]
    AC -- ya --> OK["Hapus catatan gagal<br/>hapus sesi kedaluwarsa & sesi lama"]
    OK --> TK["Token acak 256-bit<br/>DB simpan SHA-256 token"]
    TK --> CO["Set-Cookie sid (HttpOnly, SameSite=Lax,<br/>Secure di HTTPS) + flash 'Selamat datang'"]
    CO --> NX["303 → next (hanya path lokal)<br/>atau /admin"]

    REG(["POST /register"]) --> RA{"Registrasi diizinkan<br/>di Pengaturan?"}
    RA -- tidak --> R403["403"]
    RA -- ya --> RV["Validasi username, email, password ≥ 8"]
    RV --> RU["Buat user peran User"] --> RL2["303 → /login + pesan &quot;Silakan masuk&quot;"]

    OUT(["POST /logout"]) --> DS["Hapus sesi di DB + hapus cookie sid"]
```

Sesi juga dihapus saat password diganti, peran diubah, akun dinonaktifkan, atau lewat perintah
`DCMS reset-password`.

### 7. Dashboard: menu dan hak akses

```mermaid
flowchart LR
    U(["/admin<br/>(login)"]) --> D["Dashboard<br/>widget: sambutan, angka, kalender, terakhir diperbarui<br/>+ SEO & menunggu review (staf) + status server (admin)<br/>widget bisa disembunyikan per pengguna"]

    U --> HAL["Halaman"]
    U --> ART["Artikel"]
    U --> CAB["Cabang"]
    U --> SEOM["SEO & Analitik"]
    U --> APIM["API Gateway"]
    U --> USR["Pengguna"]
    U --> SET["Pengaturan"]
    U --> PRO["Profil"]

    HAL --> HAL1["daftar + filter + urutan menu<br/>buat dari templat / salin<br/>pengaturan: judul, alamat, terbit/draf, menu, ikon, SEO<br/>duplikat · hapus · Edit di halaman"]
    ART --> ART1["Semua artikel · Tulis baru<br/>editor Markdown + pratinjau + skor SEO"]
    ART --> ART2["Kategori"]
    CAB --> CAB1["CRUD cabang, koordinat, urutan, aktif"]
    SEOM --> SEO1["kunjungan per sumber, halaman terpopuler,<br/>audit SEO artikel, integrasi Google/Bing/GA4"]
    APIM --> API1["aktif/publik per API, CORS,<br/>API key (hash, cakupan, kuota), statistik"]
    USR --> USR1["CRUD pengguna & peran"]
    SET --> SET1["nama, slogan, deskripsi, registrasi,<br/>WhatsApp, sosial, logo dashboard"]
    PRO --> PRO1["profil, foto, ganti password"]

    classDef semua fill:#e2f4f1,stroke:#0f766e,color:#10201e
    classDef staf fill:#fef3c7,stroke:#b45309,color:#10201e
    classDef admin fill:#fee2e2,stroke:#b91c1c,color:#10201e
    class D,ART,ART1,PRO,PRO1 semua
    class HAL,HAL1,ART2,CAB,CAB1,SEOM,SEO1 staf
    class APIM,API1,USR,USR1,SET,SET1 admin
```

Warna: hijau = semua peran (User hanya artikel miliknya), kuning = Admin & Editor, merah = khusus Admin.
Semua pembatasan diperiksa di server (`staff`/`admin` di `Handlers.Admin`), bukan hanya disembunyikan dari
menu. Integrasi SEO (Google/Bing/GA4) hanya bisa disimpan Admin.

### 8. Page builder: edit di halaman

```mermaid
sequenceDiagram
    autonumber
    actor A as Admin/Editor
    participant B as Browser (builder.js)
    participant S as Server (Handlers)
    participant D as SQLite

    A->>S: GET /:slug?edit=1
    S->>D: findPage + settings + menu + artikel + cabang
    S-->>B: HTML halaman dengan atribut data-f / data-fa,<br/>template semua jenis blok, panel Tampilan situs
    Note over B: Edit langsung di halaman
    A->>B: ketik teks (contenteditable)
    A->>B: seret blok/item, tambah blok dari panel
    A->>B: klik gambar/tombol/ikon/chip → popover isian
    opt Unggah gambar (blok atau logo website)
        B->>S: POST /admin/upload (multipart + _csrf)
        S->>S: cek ukuran ≤ 2 MB + jenis dari isi berkas
        S-->>B: { url: "/asset/img/img-…png" }
    end
    A->>B: ubah Tema / Navigasi / Footer
    B->>B: pratinjau langsung (variabel CSS, logo, footer)
    A->>B: Simpan / Ctrl+S
    B->>B: collect(): blok → JSON { type, f, items }
    B->>S: POST /admin/pages/:slug<br/>blocks=JSON, nav=JSON (bila berubah), _csrf
    S->>S: sanitizeBlocks: jenis & isian dikenal, maks. 60 blok,<br/>URL aman, ikon, angka dibatasi
    S->>S: sanitizeNav: warna #hex, angka dalam rentang,<br/>teks dipotong, logo hanya /asset/img atau https
    S->>D: UPDATE pages.blocks + settings.nav_style
    S-->>B: { ok: true, blocks: n }
    B-->>A: "Tersimpan ✓"
```

### 9. Membuat halaman baru

```mermaid
flowchart TD
    N(["Halaman → Halaman baru"]) --> FM["Form: judul, alamat, templat atau salin halaman,<br/>status, tampil di menu, ikon, SEO"]
    FM --> SB(["POST /admin/pages"])
    SB --> SL{"Alamat diisi?"}
    SL -- tidak --> AU["slugify(judul) → cari yang belum dipakai<br/>& bukan alamat sistem (x, x-2, x-3 …)"]
    SL -- ya --> VS{"Format sah · bukan alamat sistem<br/>· belum dipakai?"}
    VS -- tidak --> ER["422 + pesan kesalahan"]
    AU & VS -- ya --> SRC{"Salin dari halaman lain?"}
    SRC -- ya --> CP["blok = salinan blok halaman itu"]
    SRC -- tidak --> TP["blok = templat:<br/>kosong · standar · landing · tentang · layanan<br/>· harga · FAQ · galeri · kontak<br/>+ judul halaman dipasang di blok judul/hero"]
    CP & TP --> INS["INSERT pages<br/>menu_order = terakhir + 1"]
    INS --> GO["303 → /:slug?edit=1<br/>langsung ke page builder"]

    subgraph Kelola["Aksi lain di daftar halaman"]
        K1["▲▼ ubah urutan menu (nomori ulang 1..n)"]
        K2["Terbitkan / jadikan draf (Beranda selalu terbit)"]
        K3["Duplikat → draf '-salinan', tidak di menu"]
        K4["Hapus (halaman bawaan tidak bisa)"]
        K5["Pengaturan: ubah judul, alamat (bukan bawaan), menu, ikon, SEO"]
    end
```

### 10. Alur artikel

```mermaid
stateDiagram-v2
    [*] --> Draf: Tulis baru
    Draf --> MenungguReview: User/Editor/Admin kirim untuk review
    MenungguReview --> Draf: dikembalikan
    Draf --> Terbit: Editor/Admin terbitkan
    MenungguReview --> Terbit: Editor/Admin terbitkan
    Terbit --> Draf: Editor/Admin tarik kembali
    Terbit --> [*]: Hapus
    Draf --> [*]: Hapus
    MenungguReview --> [*]: Hapus

    state Terbit {
        [*] --> Publik
        Publik: tampil di /post/slug, /blog, kategori,<br/>blok Artikel terbaru, sitemap, API posts
    }
```

User hanya bisa memilih Draf atau Menunggu review untuk artikelnya sendiri. Saat menulis, skor SEO (12
pemeriksaan) dan pratinjau Google dihitung langsung di browser dengan aturan yang sama seperti `CMS.Seo`.

### 11. API Gateway

```mermaid
flowchart TD
    Q(["GET /api/v1/:api/…"]) --> C1{"API ada di katalog?<br/>branches · posts · categories · site"}
    C1 -- tidak --> E404["404 not_found"]
    C1 -- ya --> C2{"API aktif?<br/>(menu API Gateway)"}
    C2 -- tidak --> E404b["404 api_disabled"]
    C2 -- ya --> K{"Key dikirim?<br/>X-API-Key · Bearer · ?api_key="}
    K -- tidak --> PUB{"API publik?"}
    PUB -- tidak --> E401["401 api_key_required"]
    PUB -- ya --> RL
    K -- ya --> HK["Cari hash SHA-256 key di api_keys"]
    HK --> V{"Ada · aktif · cakupan mengizinkan API ini?"}
    V -- tidak --> E4013["401 invalid / disabled<br/>atau 403 forbidden"]
    V -- ya --> RL{"Kuota per menit<br/>(per key, atau per IP untuk publik)"}
    RL -- habis --> E429["429 rate_limited + Retry-After"]
    RL -- ok --> EP["Endpoint:<br/>branches (q, province, city, near, radius_km, paginasi)<br/>branches/{id} · branches/provinces<br/>posts · posts/{slug} · categories · site"]
    EP --> LOG["api_usage +1 (hari, key, api, status)<br/>+ last_used_at key"]
    LOG --> RES["JSON + CORS (domain diizinkan)<br/>+ X-RateLimit-* + Cache-Control"]
    E401 & E4013 & E429 --> LOG2["catat ke api_usage"]
```

Dokumentasi interaktif ada di `/api/docs`, spesifikasi OpenAPI di `/api/openapi.json`, dan preflight CORS
dijawab di `OPTIONS /api/…`.

### 12. Unggah gambar

```mermaid
flowchart LR
    F(["POST /admin/upload<br/>/admin/profile/avatar<br/>/admin/settings/logo"]) --> SZ{"≤ 2 MB?"}
    SZ -- tidak --> X1["Tolak"]
    SZ -- ya --> MG{"Magic bytes:<br/>PNG · JPG · GIF · WebP?"}
    MG -- tidak --> X2["Tolak (SVG & lainnya)"]
    MG -- ya --> NM["Nama acak: prefix-token.ext"]
    NM --> WR["Tulis ke CMS_ASSETS"]
    WR --> URL["/asset/img/nama → disimpan di blok,<br/>profil, logo, atau logo website"]
    URL --> SRV["GET /asset/img/nama<br/>disajikan dengan CSP sandbox"]
```

### 13. Analitik kunjungan

```mermaid
flowchart LR
    RQ(["Respons GET 200 HTML"]) --> F1{"Halaman publik? tidak login?<br/>bukan ?edit=1? bukan bot?"}
    F1 -- tidak --> NO["tidak dihitung"]
    F1 -- ya --> REF["Header Referer"]
    REF --> CL{"classifyReferer"}
    CL --> S1["search + nama mesin<br/>(Google, Bing, DuckDuckGo …)"]
    CL --> S2["social (Facebook, Instagram, X, TikTok …)"]
    CL --> S3["direct · internal · referral (domain)"]
    S1 & S2 & S3 --> UP["UPSERT page_views<br/>(hari, path, sumber, mesin) views+1"]
    UP --> REP["Menu SEO & Analitik + widget dashboard:<br/>grafik harian per sumber, halaman terpopuler"]
    UP --> CLN["Data > 400 hari dihapus saat start"]
```

Tidak ada cookie pelacak, IP, atau data pribadi yang disimpan.

### 14. Skema database

```mermaid
erDiagram
    users ||--o{ sessions : "memiliki"
    users ||--o{ posts : "menulis"
    categories |o--o{ posts : "mengelompokkan"
    api_keys |o--o{ api_usage : "dicatat (key_id 0 = publik)"

    users {
        int id PK
        text username UK
        text email UK
        text password_hash "bcrypt"
        text role "admin | editor | user"
        text display_name
        text bio
        int active
        text avatar
        text dash_hidden "widget disembunyikan"
        text created_at
    }
    sessions {
        text token PK "SHA-256 token"
        int user_id FK
        text expires_at
    }
    categories {
        int id PK
        text name
        text slug UK
        text description
    }
    posts {
        int id PK
        text title
        text slug UK
        text excerpt
        text body "Markdown"
        text status "draft | pending | published"
        int author_id FK
        int category_id FK
        text seo_title
        text seo_desc
        text seo_keyword
        text seo_image
        int seo_noindex
        text created_at
        text updated_at
        text published_at
    }
    pages {
        text slug PK
        text title
        text blocks "JSON blok page builder"
        text status "published | draft"
        int in_menu
        int menu_order
        text icon
        text seo_title
        text seo_desc
        text seo_image
        int seo_noindex
        text created_at
        text updated_at
    }
    settings {
        text key PK
        text value
    }
    page_views {
        text day PK
        text path PK
        text source PK
        text engine PK
        int views
    }
    branches {
        int id PK
        text name
        text city
        text province
        text address
        text phone
        text phone2
        text whatsapp
        text hours
        text note
        text maps_url
        text maps_query
        real lat
        real lng
        int sort_order
        int active
        text updated_at
    }
    api_keys {
        int id PK
        text name
        text prefix
        text key_hash UK
        text scopes "* atau daftar API"
        int rate_limit "per menit"
        int active
        text created_at
        text last_used_at
    }
    api_usage {
        text day PK
        int key_id PK
        text api PK
        int status PK
        int hits
    }
```

Isi penting tabel `settings`:

| Kunci | Isi |
|---|---|
| `site_name`, `tagline`, `description`, `per_page`, `allow_register` | identitas & perilaku situs (menu Pengaturan) |
| `whatsapp`, `wa_message`, `social_*` | tombol WhatsApp & ikon media sosial |
| `logo` | logo dashboard (juga cadangan logo website & favicon) |
| `seo_google`, `seo_bing`, `seo_ga`, `seo_image`, `seo_index` | integrasi menu SEO |
| `nav_style` | JSON Tampilan situs dari page builder: Tema (warna, latar body & halaman, huruf, lebar, logo website), Navigasi (warna bersama, menu atas & bawah), Footer (isi, warna, ukuran, melayang, sudut) |
| `api_config` | JSON API Gateway: API aktif/publik, CORS |
| `branches_seeded`, `pages_menu_v1`, … | penanda migrasi sekali jalan |

### 15. Saat server mulai

```mermaid
flowchart TD
    S(["DCMS dijalankan"]) --> AR{"Argumen?"}
    AR -- "reset-password user" --> RP["Ganti password (CMS_NEW_PASSWORD / diketik)<br/>+ hapus semua sesinya → keluar"]
    AR -- tidak ada --> ENV["Baca env: CMS_PORT, CMS_DB, CMS_STATIC, CMS_ASSETS,<br/>CMS_SITE_URL, CMS_TRUST_PROXY, CMS_ADMIN_PASSWORD"]
    ENV --> OD["openDb: foreign_keys ON, WAL"]
    OD --> MG["migrate: CREATE TABLE IF NOT EXISTS<br/>+ ALTER TABLE kolom baru (avatar, SEO, status halaman …)<br/>+ data awal cabang + migrasi sekali jalan"]
    MG --> SP["seedPages: halaman bawaan (Home, About, Services,<br/>Branch, Blog, Contact) bila belum ada"]
    SP --> SE{"Belum ada pengguna?"}
    SE -- ya --> AD["Buat admin + kategori + artikel contoh"]
    SE -- tidak --> RUN
    AD --> RUN["Warp listen semua alamat IPv4/IPv6 :PORT<br/>log request + shutdown rapi saat SIGTERM"]
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
| GET | `/:slug` (halaman builder) | publik; draf hanya admin, editor |
| GET/POST | `/admin/pages`, `/admin/pages/new`, `/admin/pages/:slug/settings`; POST `/admin/pages/:slug/{duplicate,toggle,move,delete}` | admin, editor |
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

## Menjalankan dengan Docker

Butuh Docker Engine + plugin Compose. Semua diatur di `docker-compose.yml` (image dari `Dockerfile`).

**Coba di komputer sendiri** (tanpa HTTPS):

```bash
cp .env.example .env        # isi CMS_ADMIN_PASSWORD (opsional; tanpa itu password admin = admin12345)
docker compose up -d --build
docker compose logs -f dcms # tunggu "DCMS berjalan di port 3000"
```

Buka <http://localhost:8080/admin> dan login sebagai `admin`. Port diatur lewat `DCMS_PORT` di `.env`;
bawaan hanya bisa diakses dari komputer ini (`DCMS_BIND=127.0.0.1`).

**Server produksi dengan HTTPS** (Caddy + Let's Encrypt; domain harus sudah mengarah ke server). Di `.env`
aktifkan `DCMS_DOMAIN`, `CMS_SITE_URL=https://domain-anda`, dan `CMS_TRUST_PROXY=1`, lalu:

```bash
docker compose --profile https up -d --build
```

Perintah berguna:

```bash
docker compose ps                                # status & health
docker compose up -d --build                     # update setelah git pull
docker compose down                              # berhenti (data tetap ada di volume dcms-data)
docker cp cms.db dcms:/data/cms.db && docker restart dcms   # pakai database lama
docker compose exec -u 10001 dcms /app/DCMS reset-password admin  # lupa password (diketik)
docker run --rm -v dcms-data:/data -v "$PWD":/backup debian:bookworm-slim \
  tar czf /backup/dcms-$(date +%F).tar.gz -C /data .        # backup volume
```

Ingin memakai folder host sebagai data? Ganti `dcms-data:/data` di `docker-compose.yml` dengan mis.
`/srv/dcms:/data` (tambah `:Z` bila SELinux aktif). Pemilik isinya (termasuk `cms.db` lama) dirapikan
otomatis saat kontainer mulai.

## Deploy dengan Podman di AWS

Rekomendasi: **satu instans EC2** (Amazon Linux 2023 atau Ubuntu 24.04) dengan disk EBS. Database SQLite
butuh disk lokal yang awet, jadi ECS Fargate/App Runner tanpa disk tetap **tidak** cocok.

Berkas yang disediakan:

- `Dockerfile` — image dua tahap (build Haskell → runtime Debian slim, aplikasi berjalan sebagai user non-root, healthcheck). Dipakai Docker maupun Podman.
- `docker-compose.yml` — DCMS (+ Caddy HTTPS otomatis dengan `--profile https`), untuk Docker atau `podman compose`.
- `deploy/quadlet/` — unit systemd (Podman Quadlet): jalan otomatis saat boot dan restart bila crash.
- `deploy/Caddyfile`, `.env.example`.

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

Alternatif tanpa systemd: `cp .env.example .env` (isi), lalu `podman compose --profile https up -d`.

**Memindahkan data lama (opsional)** — salin `cms.db` dan folder gambar ke volume, lalu nyalakan:

```bash
sudo systemctl stop dcms
sudo podman volume create dcms-data
sudo cp cms.db "$(sudo podman volume inspect dcms-data --format '{{.Mountpoint}}')/cms.db"
sudo cp -r src/asset/img "$(sudo podman volume inspect dcms-data --format '{{.Mountpoint}}')/assets"
sudo systemctl start dcms     # pemilik berkas dirapikan otomatis saat kontainer mulai
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

Aplikasi berjalan sebagai user `dcms` (uid 10001), bukan root. Saat kontainer mulai, entrypoint merapikan
pemilik isi `/data` (mis. `cms.db` yang disalin sebagai root, atau folder host) lalu menurunkan hak ke user
tersebut, jadi biasanya tidak perlu apa-apa. Bila tetap muncul `attempt to write a readonly database` atau
`Permission denied`, log kontainer menampilkan `GALAT: …` beserta cara memperbaikinya:

- Jangan jalankan dengan `--user`/`user:` lain, dan jangan buang hak `CHOWN`, `SETUID`, `SETGID` (sudah
  diatur di `docker-compose.yml` dan `deploy/quadlet/dcms.container`).
- Atau ubah pemilik di host: `sudo chown -R 10001:10001 <folder-data>`.
- SELinux (Fedora/RHEL) dengan folder host: tambahkan `:Z`, mis. `-v /srv/dcms:/data:Z`.
- Pesan `Read-only file system` saat menulis ke luar `/data` itu disengaja (root filesystem hanya-baca);
  semua data aplikasi memang disimpan di `/data`.
