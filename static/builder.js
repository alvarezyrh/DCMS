// Page builder: edit teks di tempat, drag & drop blok/item, simpan ke server.
//
// Struktur yang dibaca dari HTML (dirender server, lihat CMS/Views/Site.hs):
//   [data-block="jenis"]  satu blok di dalam #pb-canvas
//   [data-f="nama"]       isian teks (contenteditable); .ml = boleh banyak baris
//   [data-fa="f:attr,…"]  isian dari atribut (URL tautan, gambar, ikon, jumlah)
//   [data-items]          daftar item; [data-item] satu item; template[data-item-tpl]
//   template[data-tpl]    blok baru dari panel
(function () {
  "use strict";

  var body = document.body;
  if (!body.classList.contains("pb-editing")) return;

  var canvas = document.getElementById("pb-canvas");
  var saveBtn = document.getElementById("pb-save");
  var statusEl = document.getElementById("pb-status");
  var csrf = document.getElementById("pb-csrf").value;
  var slug = body.getAttribute("data-page");
  var palette = document.querySelector("[data-palette]");

  var LABELS = {
    href: "Tautan tombol (URL)",
    href2: "Tautan tombol kedua (URL)",
    src: "URL gambar",
    alt: "Teks alternatif gambar",
    icon: "Ikon (kelas Font Awesome)",
    count: "Jumlah artikel (1–12)",
    interval: "Ganti slide otomatis tiap … detik (0 = mati)",
    delay: "Muncul setelah … detik (0 = langsung)",
    freq: "Seberapa sering tampil",
    layout: "Posisi gambar",
    url: "Tautan video YouTube",
    rating: "Jumlah bintang (1–5)",
    featured: "Sorot paket ini",
    style: "Gaya kartu",
    cols: "Jumlah kolom",
    shape: "Bentuk sudut kartu",
    ishape: "Bentuk ikon",
    html: "HTML",
    css: "CSS",
    js: "JavaScript"
  };
  var CODE_FIELDS = { html: 1, css: 1, js: 1 };
  var OPTIONS = {
    layout: [["right", "Di kanan teks"], ["left", "Di kiri teks"], ["bg", "Latar penuh (teks putih)"]],
    // Harus sama dengan cardVariants di CMS/Blocks.hs.
    style: [
      ["card", "Kartu (bawaan)"], ["outline", "Garis tepi"], ["lift", "Melayang (bayangan)"],
      ["left", "Ikon di samping"], ["center", "Tengah, ikon besar"], ["minimal", "Minimalis tanpa kotak"],
      ["accent", "Aksen garis atas"], ["side", "Aksen garis samping"], ["number", "Bernomor 01, 02, 03"],
      ["solid", "Ikon berwarna penuh"], ["gradient", "Gradasi warna-warni"], ["glass", "Kaca di latar lembut"],
      ["dark", "Gelap"]
    ],
    cols: [["auto", "Otomatis"], ["2", "2 kolom"], ["3", "3 kolom"], ["4", "4 kolom"]],
    featured: [["", "Tidak"], ["1", "Ya, tampilkan sebagai “Paling populer”"]],
    // Harus sama dengan popupFreqs di CMS/Blocks.hs.
    freq: [["session", "Sekali per kunjungan"], ["always", "Setiap halaman dibuka"], ["day", "Sekali sehari"], ["week", "Sekali seminggu"]],
    shape: [["rounded", "Membulat"], ["sharp", "Tajam (kotak)"], ["soft", "Sangat bulat"]],
    ishape: [["rounded", "Kotak membulat"], ["circle", "Lingkaran"], ["plain", "Tanpa latar"]]
  };

  // Sama dengan youtubeId di CMS/Blocks.hs.
  function youtubeId(url) {
    url = (url || "").trim();
    if (url.indexOf("youtu") < 0) return null;
    var marks = ["v=", "youtu.be/", "/embed/", "/shorts/", "/live/"];
    for (var i = 0; i < marks.length; i++) {
      var at = url.indexOf(marks[i]);
      if (at < 0) continue;
      var m = /^[A-Za-z0-9_-]*/.exec(url.slice(at + marks[i].length))[0];
      if (m.length === 11) return m;
    }
    return null;
  }

  function updateVideo(chip) {
    var box = chip.closest("[data-block]").querySelector("[data-video-frame]");
    var id = youtubeId(chip.getAttribute("data-url"));
    box.textContent = "";
    if (id) {
      var f = document.createElement("iframe");
      f.src = "https://www.youtube-nocookie.com/embed/" + id;
      f.title = "Video";
      f.allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share";
      f.allowFullscreen = true;
      f.referrerPolicy = "strict-origin-when-cross-origin";
      box.appendChild(f);
    } else {
      box.innerHTML = '<div class="video-empty"><i class="fa-brands fa-youtube"></i><span>' +
        (chip.getAttribute("data-url") ? "Tautan bukan video YouTube yang dikenali" : "Klik “URL video YouTube” lalu tempel tautan videonya") + "</span></div>";
    }
  }

  // Chip dengan data-variant="kunci:prefix,...": ganti kelas blok sesuai pilihan.
  function updateVariant(chip) {
    var block = chip.closest("[data-block]");
    chip.getAttribute("data-variant").split(",").forEach(function (pair) {
      var p = pair.split(":"), key = p[0], prefix = p[1];
      var v = chip.getAttribute("data-" + key);
      var opts = OPTIONS[key] || [];
      if (!opts.some(function (o) { return o[0] === v; })) v = opts.length ? opts[0][0] : "";
      Array.prototype.slice.call(block.classList).forEach(function (c) {
        if (c.indexOf(prefix) === 0) block.classList.remove(c);
      });
      block.classList.add(prefix + v);
    });
  }

  // Hero: tampilkan/sembunyikan gambar dan ganti posisinya sesuai isian chip "Gambar hero".
  function updateHero(chip) {
    var block = chip.closest("[data-block]");
    var src = chip.getAttribute("data-src") || "";
    var img = block.querySelector(".hero-img");
    if (img) {
      img.src = src || "/asset/img/placeholder.svg";
      img.alt = chip.getAttribute("data-alt") || "";
    }
    var layout = chip.getAttribute("data-layout");
    if (!OPTIONS.layout.some(function (o) { return o[0] === layout; })) layout = "right";
    block.classList.remove("hero-right", "hero-left", "hero-bg");
    block.classList.add("hero-" + layout);
    block.classList.toggle("has-img", !!src);
  }

  // Sama persis dengan codeDoc di CMS/Views/Site.hs.
  function codeDoc(html, css, js) {
    return '<!doctype html><html><head><meta charset="utf-8">' +
      '<meta name="viewport" content="width=device-width, initial-scale=1">' +
      '<base target="_top">' +
      '<style>html,body{margin:0}body{font-family:Nunito,system-ui,-apple-system,"Segoe UI",Roboto,sans-serif}</style>' +
      "<style>" + css + "</style></head><body>" + html +
      "<script>" + js + "</scr" + "ipt>" +
      '<script>(function(){function s(){parent.postMessage({dcmsCodeHeight:Math.ceil(document.documentElement.getBoundingClientRect().height)},"*")}' +
      'if(window.ResizeObserver)new ResizeObserver(s).observe(document.documentElement);addEventListener("load",s);s()})();</scr' + "ipt>" +
      "</body></html>";
  }

  // Pratinjau kode diperbarui setelah berhenti mengetik sebentar (skrip ikut dijalankan ulang).
  var codeTimer = null;
  function refreshCode(chip) {
    clearTimeout(codeTimer);
    codeTimer = setTimeout(function () {
      var block = chip.closest("[data-block]");
      var frame = block && block.querySelector("iframe[data-code-frame]");
      if (frame) frame.srcdoc = codeDoc(chip.getAttribute("data-html") || "", chip.getAttribute("data-css") || "", chip.getAttribute("data-js") || "");
    }, 450);
  }
  var ICONS = [
    "fa-solid fa-bolt", "fa-solid fa-shield-halved", "fa-solid fa-headset", "fa-solid fa-star",
    "fa-solid fa-chart-line", "fa-solid fa-gears", "fa-solid fa-code", "fa-solid fa-cloud",
    "fa-solid fa-users", "fa-solid fa-handshake", "fa-solid fa-lightbulb", "fa-solid fa-leaf",
    "fa-solid fa-truck-fast", "fa-solid fa-hand-holding-heart", "fa-solid fa-graduation-cap", "fa-solid fa-location-dot"
  ];
  var TYPE_LABELS = {};
  palette.querySelectorAll("[data-type]").forEach(function (b) {
    TYPE_LABELS[b.getAttribute("data-type")] = b.textContent.trim();
  });

  // ---------- Status & perubahan ----------
  var dirty = false;
  function setDirty(v) {
    dirty = v;
    statusEl.textContent = v ? "Ada perubahan yang belum disimpan" : "Semua perubahan tersimpan";
    statusEl.classList.toggle("is-dirty", v);
  }

  // ---------- Teks yang bisa diedit ----------
  var plainOK = (function () {
    var d = document.createElement("div");
    try { d.contentEditable = "plaintext-only"; } catch (e) { return false; }
    return d.contentEditable === "plaintext-only";
  })();

  function initEditable(scope) {
    // Tautan & gambar jangan ikut terseret bawaan browser.
    scope.querySelectorAll("a, img").forEach(function (el) { el.draggable = false; });
    scope.querySelectorAll("[data-f]").forEach(function (el) {
      if (el.getAttribute("contenteditable")) return;
      el.contentEditable = plainOK ? "plaintext-only" : "true";
      el.spellcheck = true;
    });
  }

  // Teks mentah (bukan innerText: itu ikut terkena CSS text-transform seperti uppercase).
  function rawText(node) {
    var out = "";
    node.childNodes.forEach(function (n) {
      if (n.nodeType === 3) out += n.data;
      else if (n.nodeName === "BR") out += "\n";
      else if (n.nodeType === 1) {
        if (/^(DIV|P|LI)$/.test(n.nodeName) && out && out.slice(-1) !== "\n") out += "\n";
        out += rawText(n);
      }
    });
    return out;
  }

  function textOf(el) {
    var t = rawText(el).replace(/ /g, " ");
    if (el.classList.contains("ml")) return t.replace(/\r/g, "").replace(/\n{3,}/g, "\n\n").trim();
    return t.replace(/\s+/g, " ").trim();
  }

  canvas.addEventListener("keydown", function (e) {
    var el = e.target.closest && e.target.closest("[data-f]");
    if (el && e.key === "Enter" && !el.classList.contains("ml")) e.preventDefault();
  });
  canvas.addEventListener("paste", function (e) {
    var el = e.target.closest && e.target.closest("[data-f]");
    if (!el) return;
    e.preventDefault();
    var t = (e.clipboardData || window.clipboardData).getData("text/plain");
    if (!el.classList.contains("ml")) t = t.replace(/\s*\n\s*/g, " ");
    document.execCommand("insertText", false, t);
  });
  canvas.addEventListener("input", function (e) {
    var el = e.target.closest && e.target.closest("[data-f]");
    // Kosongkan sisa <br> agar placeholder muncul lagi.
    if (el && rawText(el).trim() === "") el.innerHTML = "";
    setDirty(true);
  });

  // ---------- Toolbar blok & item ----------
  function toolBtn(act, ic, title) {
    return '<button type="button" class="pb-tbtn" data-act="' + act + '" title="' + title +
      '" aria-label="' + title + '"><i class="fa-solid ' + ic + '"></i></button>';
  }

  function decorateBlock(block) {
    if (!block.querySelector(":scope > .pb-tools")) {
      var bar = document.createElement("div");
      bar.className = "pb-ui pb-tools";
      bar.setAttribute("contenteditable", "false");
      bar.innerHTML =
        '<span class="pb-handle" title="Seret untuk memindahkan"><i class="fa-solid fa-grip-vertical"></i></span>' +
        '<span class="pb-tname"></span>' +
        toolBtn("up", "fa-arrow-up", "Naikkan") + toolBtn("down", "fa-arrow-down", "Turunkan") +
        toolBtn("dup", "fa-copy", "Duplikat") + toolBtn("del", "fa-trash", "Hapus blok");
      bar.querySelector(".pb-tname").textContent = TYPE_LABELS[block.getAttribute("data-block")] || block.getAttribute("data-block");
      block.insertBefore(bar, block.firstChild);
    }
    block.querySelectorAll("[data-items]").forEach(function (list) {
      list.querySelectorAll(":scope > [data-item]").forEach(decorateItem);
      if (!list.querySelector(":scope > .pb-add-item")) {
        var add = document.createElement("button");
        add.type = "button";
        add.className = "pb-ui pb-add-item";
        add.setAttribute("data-act", "iadd");
        add.innerHTML = '<i class="fa-solid fa-plus"></i> Tambah item';
        list.appendChild(add);
      }
    });
    initEditable(block);
  }

  function decorateItem(item) {
    if (item.querySelector(":scope > .pb-itools")) return;
    var bar = document.createElement("div");
    bar.className = "pb-ui pb-itools";
    bar.setAttribute("contenteditable", "false");
    bar.innerHTML =
      '<span class="pb-ihandle" title="Seret untuk memindahkan"><i class="fa-solid fa-grip-vertical"></i></span>' +
      toolBtn("idup", "fa-copy", "Duplikat item") + toolBtn("idel", "fa-xmark", "Hapus item");
    item.appendChild(bar);
    initEditable(item);
  }

  // Salinan bersih (tanpa tombol editor) untuk duplikat.
  function cloneClean(el) {
    var c = el.cloneNode(true);
    c.querySelectorAll(".pb-ui").forEach(function (x) { x.remove(); });
    c.querySelectorAll("[contenteditable]").forEach(function (x) { x.removeAttribute("contenteditable"); });
    c.classList.remove("pb-dragging");
    return c;
  }

  function blocks() { return Array.prototype.slice.call(canvas.querySelectorAll(":scope > [data-block]")); }

  function newBlock(type) {
    var tpl = document.querySelector('template[data-tpl="' + type + '"]');
    if (!tpl) return null;
    var node = tpl.content.firstElementChild.cloneNode(true);
    decorateBlock(node);
    return node;
  }

  function updateEmpty() { canvas.classList.toggle("is-empty", blocks().length === 0); }

  function flash(el) {
    el.classList.add("pb-flash");
    setTimeout(function () { el.classList.remove("pb-flash"); }, 900);
  }

  function focusFirst(el) {
    var f = el.querySelector("[data-f]");
    if (f) f.focus();
  }

  // ---------- Klik di kanvas: aksi, tautan, popover ----------
  canvas.addEventListener("click", function (e) {
    var btn = e.target.closest("[data-act]");
    if (btn && canvas.contains(btn)) {
      e.preventDefault();
      var block = btn.closest("[data-block]");
      var item = btn.closest("[data-item]");
      var i = blocks().indexOf(block);
      switch (btn.getAttribute("data-act")) {
        case "up":
          if (i > 0) { canvas.insertBefore(block, blocks()[i - 1]); block.scrollIntoView({ block: "nearest", behavior: "smooth" }); }
          break;
        case "down":
          var next = blocks()[i + 1];
          if (next) { canvas.insertBefore(next, block); block.scrollIntoView({ block: "nearest", behavior: "smooth" }); }
          break;
        case "dup":
          var copy = cloneClean(block);
          block.after(copy);
          decorateBlock(copy);
          flash(copy);
          break;
        case "del":
          if (!window.confirm("Hapus blok “" + (TYPE_LABELS[block.getAttribute("data-block")] || "") + "”?")) return;
          block.remove();
          break;
        case "idup":
          var ic = cloneClean(item);
          item.after(ic);
          decorateItem(ic);
          break;
        case "idel":
          item.remove();
          break;
        case "iadd":
          var list = btn.closest("[data-items]");
          var it = list.querySelector(":scope > template[data-item-tpl]");
          var node = it.content.firstElementChild.cloneNode(true);
          list.insertBefore(node, btn);
          decorateItem(node);
          focusFirst(node);
          break;
      }
      closePop();
      setDirty(true);
      updateEmpty();
      return;
    }
    var a = e.target.closest("a");
    if (a && canvas.contains(a)) e.preventDefault();
    var fa = e.target.closest("[data-fa]");
    if (fa && canvas.contains(fa)) openPop(fa);
  });

  // ---------- Popover atribut (URL, gambar, ikon, jumlah) ----------
  var pop = document.createElement("div");
  pop.className = "pb-ui pb-pop";
  pop.hidden = true;
  document.body.appendChild(pop);
  var popTarget = null;

  function applyAttr(el, attr, v) {
    if (attr === "src") el.setAttribute("src", v || "/asset/img/placeholder.svg");
    else el.setAttribute(attr, v);
    if (attr === "data-icon") {
      var i = el.querySelector("i");
      if (i) i.className = v;
    }
    if (el.hasAttribute("data-freq-chip")) {
      var fo = OPTIONS.freq.filter(function (o) { return o[0] === v; })[0];
      el.textContent = "Tampil: " + (fo ? fo[1] : v);
    }
    if (el.hasAttribute("data-label")) el.textContent = el.getAttribute("data-label") + v + (el.getAttribute("data-suffix") || "");
    if (attr === "data-html" || attr === "data-css" || attr === "data-js") refreshCode(el);
    if (el.hasAttribute("data-hero-chip")) updateHero(el);
    if (el.hasAttribute("data-variant")) updateVariant(el);
    if (el.hasAttribute("data-video")) updateVideo(el);
    // Chip "Ganti gambar" di slide: perbarui gambar latar slide-nya.
    if (attr === "data-src" || attr === "data-alt") {
      var item = el.closest("[data-item]");
      var bg = item && item.querySelector(".slide-bg");
      if (bg && attr === "data-src") bg.src = v || "/asset/img/placeholder.svg";
      if (bg && attr === "data-alt") bg.alt = v;
    }
  }

  function openPop(el) {
    if (popTarget === el && !pop.hidden) return;
    popTarget = el;
    pop.innerHTML = "";
    var fields = el.getAttribute("data-fa").split(",");
    pop.classList.toggle("pb-pop-code", fields.some(function (pair) { return CODE_FIELDS[pair.split(":")[0]]; }));
    fields.forEach(function (pair) {
      var p = pair.split(":"), field = p[0], attr = p[1];
      var lab = document.createElement("label");
      var span = document.createElement("span");
      span.textContent = LABELS[field] || field;
      var isCode = !!CODE_FIELDS[field];
      var opts = OPTIONS[field];
      var inp = document.createElement(isCode ? "textarea" : opts ? "select" : "input");
      if (opts) {
        opts.forEach(function (o) {
          var op = document.createElement("option");
          op.value = o[0];
          op.textContent = o[1];
          inp.appendChild(op);
        });
      } else if (isCode) {
        inp.rows = field === "html" ? 8 : 6;
        inp.spellcheck = false;
        inp.className = "pb-code-input";
        inp.setAttribute("autocapitalize", "off");
        inp.setAttribute("autocomplete", "off");
        // Tab menyisipkan 2 spasi, bukan pindah fokus.
        inp.addEventListener("keydown", function (e) {
          if (e.key !== "Tab" || e.shiftKey) return;
          e.preventDefault();
          document.execCommand("insertText", false, "  ");
        });
      } else inp.type = field === "count" || field === "interval" || field === "delay" ? "number" : "text";
      if (field === "count") { inp.min = 1; inp.max = 12; }
      if (field === "interval") { inp.min = 0; inp.max = 30; }
      if (field === "delay") { inp.min = 0; inp.max = 60; }
      if (field === "href" || field === "href2") inp.placeholder = "/contact atau https://…";
      if (field === "src") inp.placeholder = "https://…/gambar.jpg";
      inp.value = el.getAttribute(attr) || (opts ? opts[0][0] : "");
      inp.addEventListener(opts ? "change" : "input", function () { applyAttr(el, attr, isCode ? inp.value : inp.value.trim()); setDirty(true); });
      lab.appendChild(span);
      lab.appendChild(inp);
      pop.appendChild(lab);
      if (field === "src") pop.appendChild(uploadRow(function (url) {
        inp.value = url;
        applyAttr(el, attr, url);
        setDirty(true);
      }));
      if (field === "icon") {
        var grid = document.createElement("div");
        grid.className = "pb-icons";
        ICONS.forEach(function (ic) {
          var b = document.createElement("button");
          b.type = "button";
          b.title = ic;
          b.innerHTML = '<i class="' + ic + '"></i>';
          b.addEventListener("click", function () { inp.value = ic; applyAttr(el, attr, ic); setDirty(true); });
          grid.appendChild(b);
        });
        pop.appendChild(grid);
        var more = document.createElement("a");
        more.href = "https://fontawesome.com/search?o=r&m=free";
        more.target = "_blank";
        more.rel = "noopener";
        more.className = "pb-more";
        more.textContent = "Cari ikon lain di Font Awesome ↗";
        pop.appendChild(more);
      }
    });
    var done = document.createElement("button");
    done.type = "button";
    done.className = "btn btn-sm";
    done.textContent = "Selesai";
    done.addEventListener("click", closePop);
    pop.appendChild(done);

    pop.hidden = false;
    var r = el.getBoundingClientRect();
    var w = pop.offsetWidth;
    var left = Math.min(Math.max(8, r.left + window.scrollX), window.scrollX + document.documentElement.clientWidth - w - 8);
    pop.style.left = left + "px";
    pop.style.top = (r.bottom + window.scrollY + 8) + "px";
  }

  // Tombol "Unggah gambar": kirim ke /admin/upload, simpan di folder aset, pakai URL-nya.
  function uploadRow(done) {
    var row = document.createElement("div");
    row.className = "pb-upload";
    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "btn btn-ghost btn-sm";
    btn.innerHTML = '<i class="fa-solid fa-upload"></i> Unggah gambar';
    var note = document.createElement("small");
    note.textContent = "JPG, PNG, WebP, GIF · maks. 2 MB";
    var file = document.createElement("input");
    file.type = "file";
    file.accept = "image/png,image/jpeg,image/webp,image/gif";
    file.hidden = true;
    btn.addEventListener("click", function () { file.click(); });
    file.addEventListener("change", function () {
      var f = file.files && file.files[0];
      if (!f) return;
      var data = new FormData();
      data.append("_csrf", csrf);
      data.append("image", f);
      btn.disabled = true;
      note.textContent = "Mengunggah…";
      fetch("/admin/upload", { method: "POST", body: data, credentials: "same-origin" })
        .then(function (r) {
          var ct = r.headers.get("content-type") || "";
          if (r.ok && ct.indexOf("application/json") >= 0) return r.json();
          if (ct.indexOf("text/plain") >= 0) return r.text().then(function (t) { throw new Error(t); });
          throw new Error(r.status === 413 ? "ukuran maksimal 2 MB" : "kode " + r.status);
        })
        .then(function (res) { done(res.url); note.textContent = "Terunggah ✓"; })
        .catch(function (err) { note.textContent = "Gagal: " + err.message; })
        .then(function () { btn.disabled = false; file.value = ""; });
    });
    row.appendChild(btn);
    row.appendChild(note);
    row.appendChild(file);
    return row;
  }

  function closePop() {
    pop.hidden = true;
    popTarget = null;
  }

  document.addEventListener("mousedown", function (e) {
    if (pop.hidden) return;
    if (pop.contains(e.target) || (popTarget && popTarget.contains(e.target))) return;
    closePop();
  });
  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape") closePop();
  });

  // ---------- Drag & drop ----------
  var drag = null; // { kind: "block"|"item", el?, tpl?, list }
  var marker = document.createElement("div");
  marker.className = "pb-ui pb-marker";

  // Hanya pegangan yang membuat blok/item bisa diseret (teks tetap bisa diseleksi).
  canvas.addEventListener("mousedown", function (e) {
    var h = e.target.closest(".pb-handle, .pb-ihandle");
    if (!h) return;
    var el = h.classList.contains("pb-ihandle") ? h.closest("[data-item]") : h.closest("[data-block]");
    if (el) el.draggable = true;
  });
  document.addEventListener("mouseup", function () {
    if (drag) return;
    canvas.querySelectorAll('[draggable="true"]').forEach(function (el) { el.draggable = false; });
  });

  function startDrag(e, info) {
    drag = info;
    e.dataTransfer.effectAllowed = info.el ? "move" : "copy";
    try { e.dataTransfer.setData("text/plain", info.tpl || "pb"); } catch (err) { /* abaikan */ }
    if (info.el) setTimeout(function () { info.el.classList.add("pb-dragging"); }, 0);
    body.classList.add("pb-is-dragging");
    closePop();
  }

  canvas.addEventListener("dragstart", function (e) {
    var el = e.target;
    if (!(el instanceof Element) || !el.draggable) return;
    if (el.matches("[data-item]")) { e.stopPropagation(); startDrag(e, { kind: "item", el: el, list: el.parentElement }); }
    else if (el.matches("[data-block]")) startDrag(e, { kind: "block", el: el, list: canvas });
  });

  palette.addEventListener("dragstart", function (e) {
    var b = e.target.closest && e.target.closest("[data-type]");
    if (b) startDrag(e, { kind: "block", tpl: b.getAttribute("data-type"), list: canvas });
  });

  // Klik item panel = tambahkan di paling bawah.
  palette.addEventListener("click", function (e) {
    var b = e.target.closest("[data-type]");
    if (!b) return;
    var node = newBlock(b.getAttribute("data-type"));
    if (!node) return;
    canvas.appendChild(node);
    updateEmpty();
    setDirty(true);
    node.scrollIntoView({ behavior: "smooth", block: "center" });
    flash(node);
  });

  function placeBlockMarker(y) {
    var before = null;
    var list = blocks();
    for (var i = 0; i < list.length; i++) {
      if (list[i] === drag.el) continue;
      var r = list[i].getBoundingClientRect();
      if (y < r.top + r.height / 2) { before = list[i]; break; }
    }
    if (marker.nextSibling !== before || marker.parentNode !== canvas) canvas.insertBefore(marker, before);
  }

  // Item disusun langsung (live) di dalam daftarnya, termasuk tata letak grid.
  function placeItem(x, y) {
    var list = drag.list;
    var items = Array.prototype.slice.call(list.querySelectorAll(":scope > [data-item]"));
    var best = null, bestD = Infinity;
    items.forEach(function (it) {
      if (it === drag.el) return;
      var r = it.getBoundingClientRect();
      var d = Math.hypot(x - (r.left + r.width / 2), y - (r.top + r.height / 2));
      if (d < bestD) { bestD = d; best = it; }
    });
    if (!best) return;
    var r = best.getBoundingClientRect();
    var horizontal = list.getAttribute("data-axis") === "x" || r.width < list.getBoundingClientRect().width * 0.8;
    var after = horizontal ? x > r.left + r.width / 2 : y > r.top + r.height / 2;
    var ref = after ? best.nextElementSibling : best;
    if (ref !== drag.el && drag.el.nextElementSibling !== ref) list.insertBefore(drag.el, ref);
  }

  document.addEventListener("dragover", function (e) {
    if (!drag) return;
    if (drag.kind === "item") {
      if (!drag.list.contains(e.target)) return;
      e.preventDefault();
      placeItem(e.clientX, e.clientY);
      return;
    }
    if (!canvas.contains(e.target) && e.target !== canvas) return;
    e.preventDefault();
    e.dataTransfer.dropEffect = drag.el ? "move" : "copy";
    placeBlockMarker(e.clientY);
  });

  document.addEventListener("drop", function (e) {
    if (!drag) return;
    e.preventDefault();
    if (drag.kind === "block" && marker.parentNode === canvas) {
      var node = drag.el || newBlock(drag.tpl);
      if (node) {
        canvas.insertBefore(node, marker);
        flash(node);
      }
    }
    setDirty(true);
    endDrag();
    updateEmpty();
  });

  function endDrag() {
    if (drag && drag.el) {
      drag.el.classList.remove("pb-dragging");
      drag.el.draggable = false;
    }
    marker.remove();
    body.classList.remove("pb-is-dragging");
    drag = null;
  }
  document.addEventListener("dragend", function (e) {
    if (e.target instanceof Element && e.target.closest("[data-item]") && drag && drag.kind === "item") setDirty(true);
    endDrag();
  });

  // ---------- Navigasi atas & bawah: warna dan jarak ----------
  // Nilai disimpan sebagai { "top_bg": "#rrggbb", "top_pad": "9", ... }; kunci yang
  // tidak ada = tampilan bawaan. Variabel CSS dibuat sama seperti CMS/NavStyle.hs.
  var navPanel = document.querySelector("[data-nav-panel]");
  var navStyleEl = document.getElementById("nav-style");
  var nav = {};
  var navDirty = false;

  function rgba(hex, alpha) {
    var n = parseInt(hex.slice(1), 16);
    return "rgba(" + (n >> 16 & 255) + "," + (n >> 8 & 255) + "," + (n & 255) + "," + (alpha >= 100 ? "1" : (alpha / 100).toFixed(2)) + ")";
  }

  // Sama dengan fontFamilies di CMS/NavStyle.hs: kunci -> [font-family, parameter Google Fonts].
  var FONTS = {
    inter: ['"Inter", system-ui, sans-serif', "Inter:wght@400;500;600;700;800"],
    jakarta: ['"Plus Jakarta Sans", system-ui, sans-serif', "Plus+Jakarta+Sans:wght@400;500;600;700;800"],
    poppins: ['"Poppins", system-ui, sans-serif', "Poppins:wght@400;500;600;700;800"],
    montserrat: ['"Montserrat", system-ui, sans-serif', "Montserrat:wght@400;500;600;700;800"],
    dmsans: ['"DM Sans", system-ui, sans-serif', "DM+Sans:wght@400;500;600;700;800"],
    lora: ['"Lora", Georgia, serif', "Lora:wght@400;500;600;700"],
    playfair: ['"Playfair Display", Georgia, serif', "Playfair+Display:wght@400;500;600;700;800"]
  };
  function isLight(hex) {
    var n = parseInt(hex.slice(1), 16);
    return 0.299 * (n >> 16 & 255) + 0.587 * (n >> 8 & 255) + 0.114 * (n & 255) > 170;
  }
  function loadFont(key) {
    var f = FONTS[key];
    if (!f) return;
    var link = document.getElementById("theme-font");
    if (!link) {
      link = document.createElement("link");
      link.rel = "stylesheet";
      link.id = "theme-font";
      document.head.appendChild(link);
    }
    link.href = "https://fonts.googleapis.com/css2?family=" + f[1] + "&display=swap";
  }

  function navCss() {
    var out = "";
    if (nav.theme_primary) {
      out += "--primary:" + nav.theme_primary + ";--primary-soft:color-mix(in srgb, " + nav.theme_primary + " 14%, var(--bg));" +
        "--primary-ink:" + (isLight(nav.theme_primary) ? "#10201e" : "#ffffff") + ";";
    }
    if (nav.theme_font && FONTS[nav.theme_font]) out += "--font:" + FONTS[nav.theme_font][0] + ";";
    if (nav.theme_bg) out += "--bg:" + nav.theme_bg + ";";
    if (nav.theme_page) out += "--page-bg:" + nav.theme_page + ";";
    if (nav.page_radius != null) out += "--page-radius:" + nav.page_radius + "px;";
    if (nav.site_width != null) out += "--site-w:" + nav.site_width + "px;";
    if (nav.site_pad != null) out += "--site-pad:" + nav.site_pad + "px;";
    if (nav.logo_h != null) out += "--logo-h:" + nav.logo_h + "px;";
    // Warna navigasi dipakai bersama oleh menu atas & bawah.
    [["top", "--nt"], ["bot", "--nb"]].forEach(function (g) {
      var p = g[0], pre = g[1];
      if (nav.nav_bg) out += pre + "-bg:" + rgba(nav.nav_bg, nav.nav_opacity != null ? +nav.nav_opacity : 90) + ";";
      if (nav.nav_text) out += pre + "-text:" + nav.nav_text + ";";
      if (nav.nav_active) out += pre + "-active:" + nav.nav_active + ";";
      ["pad", "gap", "offset", "icon", "radius"].forEach(function (k) {
        if (nav[p + "_" + k] != null) out += pre + "-" + k + ":" + nav[p + "_" + k] + "px;";
      });
      // Panjang bar diatur manual (Lebar penuh tetap menang karena ditulis sesudahnya).
      if (nav[p + "_width"] != null) out += pre + "-maxw:" + nav[p + "_width"] + "px;";
      if (nav[p + "_full"] === "1") {
        out += pre + "-width:100%;" + pre + "-maxw:none;" + pre + "-track:100%;" + pre + "-side:max(var(--site-pad, 20px), calc((100% - var(--site-w, 1140px)) / 2 + var(--site-pad, 20px)));";
        if (nav[p + "_offset"] == null) out += pre + "-offset:0px;";
        if (nav[p + "_radius"] == null) out += pre + "-radius:0px;";
      }
    });
    if (nav.footer_bg) out += "--ft-bg:" + nav.footer_bg + ";";
    if (nav.footer_fg) out += "--ft-text:" + nav.footer_fg + ";";
    if (nav.footer_link) out += "--ft-link:" + nav.footer_link + ";";
    [["footer_width", "--ft-w"], ["footer_offset", "--ft-offset"], ["footer_pad", "--ft-pad"], ["footer_radius", "--ft-radius"]].forEach(function (p) {
      if (nav[p[0]] != null) out += p[1] + ":" + nav[p[0]] + "px;";
    });
    return out ? ":root{" + out + "}" : "";
  }

  // Pratinjau logo website & footer (sama dengan siteBrand/siteFooter di CMS/Views/Layout.hs).
  var fallbackLogo = navPanel ? navPanel.getAttribute("data-fallback-logo") || "" : "";
  function applyBrand() {
    var logo = nav.site_logo || fallbackLogo;
    var hideName = !!logo && nav.brand_name === "0";
    document.querySelectorAll("[data-site-brand]").forEach(function (a) {
      var img = a.querySelector(".brand-logo"), name = a.querySelector(".brand-name");
      if (logo) { img.src = logo; img.hidden = false; } else img.hidden = true;
      a.classList.toggle("has-logo", !!logo);
      name.classList.toggle("sr-only", hideName);
    });
    var prev = navPanel && navPanel.querySelector("[data-logo-preview]");
    if (prev) {
      if (logo) prev.src = logo;
      prev.hidden = !logo;
      navPanel.querySelector("[data-logo-empty]").hidden = !!logo;
    }
  }
  function applyFooter() {
    var ft = document.querySelector("[data-site-footer]");
    if (!ft) return;
    var t = ft.querySelector("[data-footer-text]"), cp = ft.querySelector("[data-footer-copy]");
    if (t) t.textContent = nav.footer_text || t.getAttribute("data-footer-text");
    if (cp) cp.textContent = (nav.footer_copy || cp.getAttribute("data-footer-copy")).split("{tahun}").join(cp.getAttribute("data-year") || "");
    ft.classList.toggle("ft-no-brand", nav.footer_brand === "0");
    ft.classList.toggle("ft-no-menu", nav.footer_menu === "0");
    ft.classList.toggle("ft-no-social", nav.footer_social === "0");
    ft.classList.toggle("ft-center", nav.footer_layout === "center");
    ft.classList.toggle("ft-float", nav.footer_float === "1");
  }

  function navVal(input) {
    return input.type === "checkbox" ? (input.checked ? "1" : "0") : input.value;
  }
  // Pilihan tanpa status "bawaan" terpisah: checkbox & select.
  function navPlain(input) { return input.type === "checkbox" || input.tagName === "SELECT" || navIsText(input); }
  // Isian teks & URL logo: kosong = bawaan (tidak disimpan).
  function navIsText(input) { return input.tagName === "TEXTAREA" || input.type === "text"; }

  function navShow(row, input) {
    var out = row.querySelector("[data-nav-out]");
    if (!out) return;
    var key = input.getAttribute("data-nav-key");
    if (!(key in nav)) out.textContent = "bawaan";
    else out.textContent = input.value + (input.getAttribute("data-unit") || "");
  }

  function navChanged() {
    if (navStyleEl) navStyleEl.textContent = navCss();
    applyBrand();
    applyFooter();
    body.toggleAttribute("data-page-box", !!nav.theme_page);
    body.classList.toggle("pb-bot-off", nav.bot_enabled === "0");
    if (nav.theme_font) loadFont(nav.theme_font);
    navDirty = true;
    setDirty(true);
  }

  function navClear(input) {
    delete nav[input.getAttribute("data-nav-key")];
    if (input.type === "checkbox") { input.checked = input.getAttribute("data-default") === "1"; return; }
    if (input.tagName === "SELECT") { input.value = input.getAttribute("data-default"); return; }
    if (navIsText(input)) { input.value = ""; return; }
    var row = input.closest(".pb-nav-row");
    if (input.type === "range") input.value = input.getAttribute("data-default");
    row.setAttribute("data-unset", "");
    navShow(row, input);
  }

  if (navPanel) {
    navPanel.querySelectorAll("[data-nav-key]").forEach(function (input) {
      var row = input.closest(".pb-nav-row");
      if (navPlain(input)) {
        if (navVal(input) !== input.getAttribute("data-default")) nav[input.getAttribute("data-nav-key")] = navVal(input);
      } else if (!row.hasAttribute("data-unset")) nav[input.getAttribute("data-nav-key")] = input.value;
    });
    body.classList.toggle("pb-bot-off", nav.bot_enabled === "0");
    navPanel.addEventListener("input", function (e) {
      var input = e.target.closest && e.target.closest("[data-nav-key]");
      if (!input) return;
      nav[input.getAttribute("data-nav-key")] = navVal(input);
      if (navIsText(input) && !input.value.trim()) delete nav[input.getAttribute("data-nav-key")];
      var row = input.closest(".pb-nav-row");
      if (row) {
        row.removeAttribute("data-unset");
        navShow(row, input);
      }
      navChanged();
    });
    navPanel.addEventListener("click", function (e) {
      var clear = e.target.closest("[data-nav-clear]");
      if (clear) {
        navClear(navPanel.querySelector('[data-nav-key="' + clear.getAttribute("data-nav-clear") + '"]'));
        navChanged();
        return;
      }
      var reset = e.target.closest("[data-nav-reset]");
      if (reset) {
        var g = reset.closest("[data-nav-group]");
        g.querySelectorAll("[data-nav-key]").forEach(navClear);
        navChanged();
      }
    });
    // Tombol unggah logo website (memakai /admin/upload seperti gambar blok).
    navPanel.querySelectorAll("[data-nav-image]").forEach(function (box) {
      var input = box.querySelector("[data-nav-key]");
      box.insertBefore(uploadRow(function (url) {
        input.value = url;
        input.dispatchEvent(new Event("input", { bubbles: true }));
      }), input.nextSibling);
    });
    // Navigasi bawah tersembunyi saat mengedit; tampilkan selama panelnya dibuka.
    var botGroup = navPanel.querySelector('[data-nav-group="nav"]');
    if (botGroup) botGroup.addEventListener("toggle", function () {
      body.classList.toggle("pb-preview-bot", botGroup.open);
    });
  }

  // ---------- Simpan ----------
  function fieldsOf(scope, owner) {
    var f = {};
    scope.querySelectorAll("[data-f]").forEach(function (el) {
      if (el.closest("[data-item]") !== owner) return;
      f[el.getAttribute("data-f")] = textOf(el);
    });
    scope.querySelectorAll("[data-fa]").forEach(function (el) {
      if (el.closest("[data-item]") !== owner) return;
      el.getAttribute("data-fa").split(",").forEach(function (pair) {
        var p = pair.split(":");
        f[p[0]] = (el.getAttribute(p[1]) || "").trim();
      });
    });
    return f;
  }

  function collect() {
    return blocks().map(function (b) {
      return {
        type: b.getAttribute("data-block"),
        f: fieldsOf(b, null),
        items: Array.prototype.map.call(b.querySelectorAll("[data-item]"), function (it) { return fieldsOf(it, it); })
      };
    });
  }

  var saving = false;
  function save() {
    if (saving) return;
    saving = true;
    saveBtn.disabled = true;
    statusEl.textContent = "Menyimpan…";
    var data = new URLSearchParams();
    data.set("_csrf", csrf);
    data.set("blocks", JSON.stringify(collect()));
    var sentNav = navDirty;
    if (sentNav) { data.set("nav", JSON.stringify(nav)); navDirty = false; }
    fetch("/admin/pages/" + encodeURIComponent(slug), { method: "POST", body: data, credentials: "same-origin" })
      .then(function (r) {
        var ct = r.headers.get("content-type") || "";
        if (r.ok && ct.indexOf("application/json") >= 0) return r.json();
        if (ct.indexOf("text/plain") >= 0) return r.text().then(function (t) { throw new Error(t); });
        throw new Error(r.status === 403 ? "sesi atau token kedaluwarsa, muat ulang halaman" : "kode " + r.status);
      })
      .then(function () {
        setDirty(navDirty);
        if (!navDirty) statusEl.textContent = "Tersimpan ✓";
      })
      .catch(function (err) {
        if (sentNav) navDirty = true;
        statusEl.textContent = "Gagal menyimpan: " + err.message;
        statusEl.classList.add("is-dirty");
      })
      .then(function () {
        saving = false;
        saveBtn.disabled = false;
      });
  }

  saveBtn.addEventListener("click", save);
  document.addEventListener("keydown", function (e) {
    if ((e.ctrlKey || e.metaKey) && e.key.toLowerCase() === "s") {
      e.preventDefault();
      save();
    }
  });
  window.addEventListener("beforeunload", function (e) {
    if (dirty) { e.preventDefault(); e.returnValue = ""; }
  });

  // ---------- Mulai ----------
  blocks().forEach(decorateBlock);
  updateEmpty();
})();
