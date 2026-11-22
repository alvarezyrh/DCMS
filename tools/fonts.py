#!/usr/bin/env python3
"""Buat ulang font yang disajikan sendiri dari static/ (tanpa CDN saat halaman dibuka):

- static/fonts.css      @font-face Nunito + kelas ikon yang dipakai
- static/nunito-*.woff2 Nunito (variable, subset latin & latin-ext) dari Google Fonts
- static/fa-*.woff2     Font Awesome Free 6.5.2, hanya glyph yang dipakai kode & database

Jalankan dari mana saja setelah menambah ikon baru di kode:

    pip install fonttools brotli
    python3 tools/fonts.py [path/cms.db]

Ikon di luar subset (mis. dipilih lewat page builder) tetap tampil: app.js memuat Font
Awesome lengkap dari CDN bila menemukan ikon yang belum ada glyph-nya.
Lisensi: font Font Awesome Free & Nunito SIL OFL 1.1, CSS Font Awesome MIT.
"""
import io
import pathlib
import re
import sqlite3
import sys
import urllib.request

from fontTools import subset
from fontTools.ttLib import TTFont

FA = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.2"
NUNITO = "https://fonts.googleapis.com/css2?family=Nunito:wght@400..800&display=swap"
UA = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
ROOT = pathlib.Path(__file__).resolve().parent.parent
STATIC = ROOT / "static"
FA_FONTS = {"solid": "fa-solid-900", "regular": "fa-regular-400", "brands": "fa-brands-400"}


def fetch(url):
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=60) as r:
        return r.read()


def used_icon_names(db):
    """Semua token fa-* di kode Haskell, JS, dan (bila ada) isi database."""
    pattern = re.compile(r"fa-[a-z0-9]+(?:-[a-z0-9]+)*")
    names = set()
    for f in list((ROOT / "uye").rglob("*.hs")) + list(STATIC.glob("*.js")):
        names |= set(pattern.findall(f.read_text(encoding="utf-8")))
    if db and pathlib.Path(db).exists():
        con = sqlite3.connect(f"file:{db}?mode=ro", uri=True)
        for (text,) in con.execute("SELECT blocks || ' ' || icon FROM pages UNION ALL SELECT value FROM settings"):
            names |= set(pattern.findall(text or ""))
    return names


def icon_glyphs(names):
    """{nama ikon: kode glyph} dari CSS Font Awesome, hanya untuk nama yang dipakai."""
    css = fetch(f"{FA}/css/all.css").decode()
    table = dict(re.findall(r'\.fa-([a-z0-9-]+)::?before\s*\{\s*content:\s*"\\([0-9a-f]+)"', css))
    return {n: table[n[3:]] for n in sorted(names) if n[3:] in table}


def subset_woff2(data, codepoints):
    font = TTFont(io.BytesIO(data))
    opts = subset.Options()
    opts.flavor = "woff2"
    opts.layout_features = ["*"]
    sub = subset.Subsetter(opts)
    sub.populate(unicodes=codepoints)
    sub.subset(font)
    out = io.BytesIO()
    font.flavor = "woff2"
    font.save(out)
    return out.getvalue()


def nunito_faces():
    """Unduh woff2 Nunito latin & latin-ext; kembalikan aturan @font-face-nya."""
    css = fetch(NUNITO).decode()
    faces = []
    for subset_name, url, ranges in re.findall(r"/\* ([a-z-]+) \*/\s*@font-face\s*\{[^}]*?src: url\(([^)]+)\)[^}]*?unicode-range: ([^;]+);", css):
        if subset_name not in ("latin", "latin-ext"):
            continue
        name = f"nunito-{subset_name}.woff2"
        (STATIC / name).write_bytes(fetch(url))
        faces.append(
            f'@font-face {{ font-family: "Nunito"; font-style: normal; font-weight: 400 800; font-display: swap;\n'
            f'  src: url({name}) format("woff2"); unicode-range: {ranges}; }}'
        )
    if len(faces) != 2:
        sys.exit("Gagal membaca CSS Nunito dari Google Fonts.")
    return faces


def main():
    db = sys.argv[1] if len(sys.argv) > 1 else ROOT / "cms.db"
    glyphs = icon_glyphs(used_icon_names(db))
    codepoints = sorted({int(cp, 16) for cp in glyphs.values()})
    for style, base in FA_FONTS.items():
        (STATIC / f"fa-{style}.woff2").write_bytes(subset_woff2(fetch(f"{FA}/webfonts/{base}.woff2"), codepoints))
    rules = "\n".join(f'.fa-{n[3:]}::before {{ content: "\\{cp}"; }}' for n, cp in glyphs.items())
    (STATIC / "fonts.css").write_text(
        "/* Dibuat oleh tools/fonts.py, jangan diedit manual.\n"
        "   Nunito (SIL OFL 1.1) & Font Awesome Free 6.5.2 (font SIL OFL 1.1, CSS MIT), subset. */\n"
        + "\n".join(nunito_faces())
        + "\n"
        '@font-face { font-family: "DCMS Icons"; font-weight: 900; font-display: block; src: url(fa-solid.woff2) format("woff2"); }\n'
        '@font-face { font-family: "DCMS Icons"; font-weight: 400; font-display: block; src: url(fa-regular.woff2) format("woff2"); }\n'
        '@font-face { font-family: "DCMS Brands"; font-weight: 400; font-display: block; src: url(fa-brands.woff2) format("woff2"); }\n'
        ".fa-solid, .fa-regular, .fa-brands { -moz-osx-font-smoothing: grayscale; -webkit-font-smoothing: antialiased;\n"
        "  display: var(--fa-display, inline-block); font-style: normal; font-variant: normal; line-height: 1; text-rendering: auto; }\n"
        '.fa-solid { font-family: "DCMS Icons"; font-weight: 900; }\n'
        '.fa-regular { font-family: "DCMS Icons"; font-weight: 400; }\n'
        '.fa-brands { font-family: "DCMS Brands"; font-weight: 400; }\n'
        + rules
        + "\n",
        encoding="utf-8",
    )
    print(f"{len(glyphs)} ikon, {len(codepoints)} glyph ditulis ke {STATIC}")


if __name__ == "__main__":
    main()
