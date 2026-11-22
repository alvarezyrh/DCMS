// Konfirmasi hapus, toolbar Markdown, pratinjau, hitung kata, Ctrl+S.
(function () {
  "use strict";

  // ---------- Ikon ----------
  // static/fonts.css hanya berisi ikon yang dipakai situs. Bila halaman memuat ikon lain
  // (mis. dipilih lewat page builder), atau sedang di mode edit (pemilih ikon bebas),
  // Font Awesome lengkap dimuat dari CDN.
  var needFullIcons = document.body.classList.contains("pb-editing") ||
    Array.prototype.some.call(document.querySelectorAll("i.fa-solid, i.fa-regular, i.fa-brands"), function (i) {
      var c = getComputedStyle(i, "::before").content;
      return !c || c === "none" || c === "normal";
    });
  if (needFullIcons) {
    var fa = document.createElement("link");
    fa.rel = "stylesheet";
    fa.href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.2/css/all.min.css";
    fa.crossOrigin = "anonymous";
    fa.referrerPolicy = "no-referrer";
    document.head.appendChild(fa);
  }

  // Form dengan data-confirm meminta konfirmasi sebelum dikirim.
  document.addEventListener("submit", function (e) {
    var form = e.target;
    var msg = form.getAttribute("data-confirm");
    if (msg && !window.confirm(msg)) e.preventDefault();
  });

  // ---------- Menu HP: tombol ☰ membuka panel menu di header ----------
  var siteHdr = document.querySelector("[data-site-header]");
  var navTog = siteHdr && siteHdr.querySelector("[data-nav-toggle]");
  if (navTog) {
    var menuOpen = false;
    var setMenu = function (on) {
      menuOpen = on;
      siteHdr.classList.toggle("is-open", on);
      navTog.setAttribute("aria-expanded", on ? "true" : "false");
      navTog.setAttribute("aria-label", on ? "Tutup menu" : "Buka menu");
    };
    navTog.addEventListener("click", function () { setMenu(!menuOpen); });
    siteHdr.querySelectorAll(".site-menu a").forEach(function (a) {
      a.addEventListener("click", function () { setMenu(false); });
    });
    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape" && menuOpen) { setMenu(false); navTog.focus(); }
    });
    document.addEventListener("click", function (e) {
      if (menuOpen && !siteHdr.contains(e.target)) setMenu(false);
    });
    if (window.matchMedia) {
      var wide = window.matchMedia("(min-width: 861px)");
      var onWide = function () { if (wide.matches) setMenu(false); };
      if (wide.addEventListener) wide.addEventListener("change", onWide);
    }
  }

  // Navigasi bawah ala iOS: header atas menghilang dan tab bar muncul saat digulir ke bawah.
  var tabbar = document.querySelector(".tabbar");
  var siteHeader = document.querySelector(".site .site-header");
  if (tabbar && !document.body.classList.contains("pb-editing")) {
    var docked = null, ticking = false;
    var update = function () {
      ticking = false;
      var hdrOpen = document.querySelector("[data-site-header].is-open");
      var want = window.scrollY > 160 && !hdrOpen;
      if (want === docked) return;
      docked = want;
      document.body.classList.toggle("nav-docked", docked);
      tabbar.inert = !docked;
      if (siteHeader) siteHeader.inert = docked;
    };
    window.addEventListener("scroll", function () {
      if (!ticking) { ticking = true; window.requestAnimationFrame(update); }
    }, { passive: true });
    update();
  }

  // Foto profil: langsung diunggah begitu file dipilih (tanpa klik tombol lagi).
  document.querySelectorAll("[data-avatar-input]").forEach(function (input) {
    var form = input.form;
    var submit = form.querySelector("button[type=submit]");
    if (submit) submit.hidden = true;
    input.addEventListener("change", function () {
      var f = input.files && input.files[0];
      if (!f) return;
      var label = input.parentElement.querySelector("[data-file-label]");
      if (f.size > 2 * 1024 * 1024) {
        if (label) label.textContent = "Maksimal 2 MB — pilih foto lain";
        input.value = "";
        return;
      }
      if (label) label.textContent = "Mengunggah…";
      var av = form.closest(".avatar-card").querySelector(".avatar");
      if (av && /^image\//.test(f.type)) {
        var img = av.querySelector("img") || document.createElement("img");
        img.alt = "";
        img.src = URL.createObjectURL(f);
        av.textContent = "";
        av.appendChild(img);
      }
      form.requestSubmit();
    });
  });

  // Blok kode custom: iframe sandbox melaporkan tinggi isinya lewat postMessage.
  window.addEventListener("message", function (e) {
    var h = e.data && e.data.dcmsCodeHeight;
    if (typeof h !== "number") return;
    document.querySelectorAll("iframe[data-code-frame]").forEach(function (f) {
      if (f.contentWindow === e.source) f.style.height = Math.min(Math.max(h, 24), 6000) + "px";
    });
  });

  // ---------- Popup brosur/promo (blok "Popup gambar") ----------
  // Muncul setelah data-delay detik. data-freq: session = sekali per kunjungan, day/week =
  // sekali sehari/seminggu (disimpan di browser), always = setiap halaman dibuka.
  if (!document.body.classList.contains("pb-editing")) {
    document.querySelectorAll("dialog[data-img-popup]").forEach(function (dlg, i) {
      var freq = dlg.getAttribute("data-freq") || "session";
      var img = dlg.querySelector(".img-popup-img");
      var key = "dcms-popup:" + location.pathname + ":" + i + ":" + (img ? img.getAttribute("src") : "");
      var span = { day: 864e5, week: 7 * 864e5 }[freq];
      var store = function () {
        try { return freq === "session" ? window.sessionStorage : window.localStorage; } catch (e) { return null; }
      };
      if (freq !== "always") {
        try {
          var last = +(store() && store().getItem(key)) || 0;
          if (last && (freq === "session" || Date.now() - last < span)) return;
        } catch (e) { /* penyimpanan diblokir: tetap tampilkan */ }
      }
      var close = function () { if (dlg.open) dlg.close ? dlg.close() : dlg.removeAttribute("open"); };
      dlg.addEventListener("click", function (e) {
        // Klik tombol ×, atau area gelap di luar kotak, menutup popup.
        if (e.target === dlg || e.target.closest("[data-popup-close]")) close();
      });
      setTimeout(function () {
        if (typeof dlg.showModal === "function") dlg.showModal(); else dlg.setAttribute("open", "");
        try { if (freq !== "always" && store()) store().setItem(key, String(Date.now())); } catch (e) { /* abaikan */ }
      }, Math.max(0, +dlg.getAttribute("data-delay") || 0) * 1000);
    });
  }

  // ---------- Popup gambar ----------
  // Gambar konten bisa diklik untuk dibuka besar. Aktif bila <body data-lightbox> (bisa
  // dimatikan di page builder) dan tidak sedang mengedit halaman.
  var LB_SEL = ".pb-img, .split-img, .pb-hero:not(.hero-bg) .hero-img, .gallery-img, .prose img";
  if (document.body.hasAttribute("data-lightbox") && !document.body.classList.contains("pb-editing")) {
    var lb = null, lbImgs = [], lbIdx = 0, lbBack = null;
    var lbOk = function (img) { return !img.closest("a") && img.getAttribute("src"); };
    var lbBuild = function () {
      lb = document.createElement("div");
      lb.className = "lightbox";
      lb.hidden = true;
      lb.setAttribute("role", "dialog");
      lb.setAttribute("aria-modal", "true");
      lb.setAttribute("aria-label", "Gambar");
      lb.innerHTML =
        '<button type="button" class="lb-btn lb-close" data-lb="close" aria-label="Tutup"><i class="fa-solid fa-xmark"></i></button>' +
        '<button type="button" class="lb-btn lb-prev" data-lb="prev" aria-label="Gambar sebelumnya"><i class="fa-solid fa-chevron-left"></i></button>' +
        '<figure class="lb-figure"><img class="lb-img" alt=""><figcaption class="lb-cap"></figcaption></figure>' +
        '<button type="button" class="lb-btn lb-next" data-lb="next" aria-label="Gambar berikutnya"><i class="fa-solid fa-chevron-right"></i></button>' +
        '<span class="lb-count"></span>';
      document.body.appendChild(lb);
      lb.addEventListener("click", function (e) {
        var b = e.target.closest("[data-lb]");
        if (b) { lbAct(b.getAttribute("data-lb")); return; }
        if (!e.target.closest(".lb-img")) lbClose(); // klik area gelap = tutup
      });
    };
    var lbShow = function (i) {
      lbIdx = (i + lbImgs.length) % lbImgs.length;
      var src = lbImgs[lbIdx];
      var big = lb.querySelector(".lb-img");
      big.src = src.currentSrc || src.src;
      big.alt = src.alt || "";
      var fig = src.closest("figure, .gallery-item");
      var cap = fig && fig.querySelector("figcaption, .gallery-cap");
      lb.querySelector(".lb-cap").textContent = (cap && cap.textContent.trim()) || src.alt || "";
      var many = lbImgs.length > 1;
      lb.classList.toggle("lb-single", !many);
      lb.querySelector(".lb-count").textContent = many ? (lbIdx + 1) + " / " + lbImgs.length : "";
    };
    var lbOpen = function (img) {
      if (!lb) lbBuild();
      lbImgs = Array.prototype.filter.call(document.querySelectorAll(LB_SEL), lbOk);
      lbBack = document.activeElement;
      lbShow(Math.max(0, lbImgs.indexOf(img)));
      lb.hidden = false;
      document.documentElement.classList.add("lb-open");
      lb.querySelector(".lb-close").focus();
    };
    var lbClose = function () {
      lb.hidden = true;
      document.documentElement.classList.remove("lb-open");
      if (lbBack && lbBack.focus) lbBack.focus();
    };
    var lbAct = function (a) {
      if (a === "close") lbClose();
      else if (a === "prev") lbShow(lbIdx - 1);
      else if (a === "next") lbShow(lbIdx + 1);
    };
    // Tandai gambar yang bisa diklik (kursor zoom & bisa dibuka dengan keyboard).
    document.querySelectorAll(LB_SEL).forEach(function (img) {
      if (!lbOk(img)) return;
      img.classList.add("lb-zoomable");
      img.tabIndex = 0;
      img.setAttribute("role", "button");
      img.setAttribute("aria-label", "Perbesar gambar" + (img.alt ? ": " + img.alt : ""));
    });
    document.addEventListener("click", function (e) {
      var img = e.target.closest && e.target.closest("img.lb-zoomable");
      if (img) { e.preventDefault(); lbOpen(img); }
    });
    document.addEventListener("keydown", function (e) {
      if (lb && !lb.hidden) {
        if (e.key === "Escape") lbClose();
        else if (e.key === "ArrowLeft" && lbImgs.length > 1) lbShow(lbIdx - 1);
        else if (e.key === "ArrowRight" && lbImgs.length > 1) lbShow(lbIdx + 1);
        else if (e.key === "Tab") {
          // Fokus tetap di dalam popup.
          var f = Array.prototype.filter.call(lb.querySelectorAll("button"), function (b) { return b.offsetParent !== null; });
          var i = f.indexOf(document.activeElement);
          e.preventDefault();
          f[(i + (e.shiftKey ? -1 : 1) + f.length) % f.length].focus();
        }
        return;
      }
      var img = e.target.closest && e.target.closest("img.lb-zoomable");
      if (img && (e.key === "Enter" || e.key === " ")) { e.preventDefault(); lbOpen(img); }
    });
  }

  // ---------- Animasi muncul saat digulir ----------
  // Hanya elemen yang belum terlihat saat halaman dibuka yang dianimasikan, jadi bagian
  // atas halaman tidak berkedip. Tanpa JS / IntersectionObserver semuanya tampil biasa.
  var reduceMotion = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  if (document.body.hasAttribute("data-reveal") && !document.body.classList.contains("pb-editing") &&
      !reduceMotion && "IntersectionObserver" in window) {
    var GRID = ".pb-grid, .pb-stats-grid, .gallery-grid, .logo-row, .faq-list, .hero-grid, .split, .contact";
    var targets = [];
    document.querySelectorAll(".site-main .pb > .wrap").forEach(function (wrap) {
      Array.prototype.forEach.call(wrap.children, function (el) {
        if (el.tagName === "TEMPLATE") return;
        // Daftar (kartu, galeri, dst.): tiap isinya muncul bergantian.
        if (el.matches(GRID)) {
          Array.prototype.forEach.call(el.children, function (child, i) {
            if (child.tagName !== "TEMPLATE") targets.push([child, Math.min(i, 6) * 0.08]);
          });
        } else targets.push([el, 0]);
      });
    });
    var vh = window.innerHeight;
    var io = new IntersectionObserver(function (entries) {
      entries.forEach(function (en) {
        if (!en.isIntersecting) return;
        en.target.classList.add("rv-in");
        io.unobserve(en.target);
      });
    }, { rootMargin: "0px 0px -8% 0px", threshold: 0.08 });
    targets.forEach(function (t) {
      var el = t[0];
      if (el.getBoundingClientRect().top < vh * 0.92) return; // sudah terlihat: biarkan
      el.classList.add("rv");
      if (t[1]) el.style.transitionDelay = t[1] + "s";
      io.observe(el);
    });
  }

  // ---------- Judul: h1 per huruf, h2 teracak lalu terbaca ----------
  // h1 di halaman situs dipecah per huruf: jatuh berputar saat dimuat, bergelombang saat
  // disorot (atau saat menerima event "hx:wave"). Huruf di dalam teks sorot (.hl) diberi
  // gradasi warna tema. Teks asli dipindah ke span.sr-only untuk pembaca layar; salinan
  // per huruf aria-hidden. h2 judul bagian page builder teracak sebentar saat terlihat.
  // Tidak berlaku di mode edit dan tidak menyentuh isi artikel.
  if (document.body.classList.contains("site") && !document.body.classList.contains("pb-editing")) {
    var splitHeading = function (h) {
      var sr = document.createElement("span");
      sr.className = "sr-only";
      while (h.firstChild) sr.appendChild(h.firstChild);
      var view = sr.cloneNode(true);
      view.className = "hx-letters";
      view.setAttribute("aria-hidden", "true");
      var letters = [];
      (function walk(node) {
        Array.prototype.slice.call(node.childNodes).forEach(function (n) {
          if (n.nodeType === 1) return walk(n);
          if (n.nodeType !== 3) return;
          var frag = document.createDocumentFragment(), word = null;
          Array.from(n.nodeValue).forEach(function (ch) {
            if (/\s/.test(ch)) { frag.appendChild(document.createTextNode(ch)); word = null; return; }
            if (!word) { word = document.createElement("span"); word.className = "hx-w"; frag.appendChild(word); }
            var c = document.createElement("span");
            c.className = "hx-ch";
            c.textContent = ch;
            word.appendChild(c);
            letters.push(c);
          });
          n.parentNode.replaceChild(frag, n);
        });
      })(view);
      // Jeda antarhuruf menyesuaikan panjang judul: total animasi maks. ~1,4 detik.
      var step = Math.min(55, 1400 / Math.max(letters.length, 1));
      letters.forEach(function (c, i) { c.style.setProperty("--d", Math.round(i * step) + "ms"); });
      view.querySelectorAll(".hl").forEach(function (hl) {
        var cs = hl.querySelectorAll(".hx-ch");
        cs.forEach(function (c, k) { c.style.setProperty("--p", cs.length > 1 ? Math.round(k / (cs.length - 1) * 100) : 0); });
      });
      h.appendChild(sr);
      h.appendChild(view);
      h.classList.add("hx-split");
      if (reduceMotion) return;
      h.classList.add("hx-drop");
      var waving = false;
      var wave = function () {
        if (waving) return;
        waving = true;
        h.classList.remove("hx-drop");
        h.classList.add("hx-wave");
        setTimeout(function () { h.classList.remove("hx-wave"); waving = false; }, letters.length * step * 0.8 + 800);
      };
      h.addEventListener("mouseenter", wave);
      h.addEventListener("hx:wave", wave);
    };
    document.querySelectorAll("main h1").forEach(splitHeading);

    if (!reduceMotion && "IntersectionObserver" in window) {
      var GLYPHS = "<>/{}[]=+*#$_01";
      var scramble = function (el) {
        var nodes = [];
        (function walk(n) {
          if (n.nodeType === 3 && n.nodeValue.trim()) nodes.push({ node: n, text: n.nodeValue });
          else Array.prototype.forEach.call(n.childNodes, walk);
        })(el);
        var start = null;
        requestAnimationFrame(function frame(t) {
          if (start === null) start = t;
          var k = Math.min((t - start) / 900, 1);
          nodes.forEach(function (o) {
            var keep = Math.floor(k * o.text.length), out = o.text.slice(0, keep);
            for (var i = keep; i < o.text.length; i++)
              out += /\s/.test(o.text[i]) ? o.text[i] : GLYPHS[(Math.random() * GLYPHS.length) | 0];
            o.node.nodeValue = out;
          });
          if (k < 1) requestAnimationFrame(frame);
        });
      };
      var heads = new IntersectionObserver(function (entries) {
        entries.forEach(function (en) {
          if (!en.isIntersecting) return;
          scramble(en.target);
          heads.unobserve(en.target);
        });
      }, { threshold: 0.6 });
      document.querySelectorAll("main .pb h2").forEach(function (h) { heads.observe(h); });
    }
  }

  // ---------- Angka (blok "Angka"): hitung naik saat terlihat ----------
  // Hanya nilai bilangan bulat dengan akhiran tanpa angka ("120+", "1.500", "98%").
  // Nilai seperti "24/7" dibiarkan apa adanya.
  if (!document.body.classList.contains("pb-editing") && !reduceMotion && "IntersectionObserver" in window) {
    var countIo = new IntersectionObserver(function (entries) {
      entries.forEach(function (en) {
        if (!en.isIntersecting) return;
        countIo.unobserve(en.target);
        var el = en.target, d = el._count, t0 = null;
        var step = function (t) {
          if (t0 === null) t0 = t;
          var p = Math.min((t - t0) / 1400, 1), v = Math.round(d.n * (1 - Math.pow(1 - p, 3)));
          el.textContent = d.pre + (d.sep ? String(v).replace(/\B(?=(\d{3})+(?!\d))/g, d.sep) : v) + d.suf;
          if (p < 1) requestAnimationFrame(step);
        };
        requestAnimationFrame(step);
      });
    }, { threshold: 0.4 });
    document.querySelectorAll(".pb-stat strong").forEach(function (el) {
      var m = el.textContent.trim().match(/^(\D*?)(\d{1,3}(?:([.,])\d{3})+|\d+)(\D*)$/);
      if (!m) return;
      var n = parseInt(m[2].replace(/[.,]/g, ""), 10);
      if (!(n > 0)) return;
      el._count = { pre: m[1], n: n, sep: m[3] || "", suf: m[4] };
      el.textContent = m[1] + "0" + m[4];
      countIo.observe(el);
    });
  }

  // ---------- Keahlian: bar progres terisi saat bagiannya terlihat ----------
  if (!document.body.classList.contains("pb-editing") && !reduceMotion && "IntersectionObserver" in window) {
    document.querySelectorAll(".skill-grid").forEach(function (grid) {
      grid.classList.add("sk-wait");
      var io = new IntersectionObserver(function (en) {
        if (!en[0].isIntersecting) return;
        grid.classList.remove("sk-wait");
        io.disconnect();
      }, { threshold: 0.3 });
      io.observe(grid);
    });
  }

  // ---------- Sorot kartu mengikuti kursor ----------
  if (!document.body.classList.contains("pb-editing") && window.matchMedia("(hover: hover)").matches) {
    document.addEventListener("pointermove", function (e) {
      var card = e.target.closest && e.target.closest(".pb-card");
      if (!card) return;
      var r = card.getBoundingClientRect();
      card.style.setProperty("--mx", (e.clientX - r.left) + "px");
      card.style.setProperty("--my", (e.clientY - r.top) + "px");
    }, { passive: true });
  }

  // Sorotan meluncur ke tab aktif (.pt-glider); di HP daftar tab ikut digeser.
  function glide(list, t) {
    var g = list.querySelector(".pt-glider");
    list.classList.add("has-glider");
    g.style.width = t.offsetWidth + "px";
    g.style.height = t.offsetHeight + "px";
    g.style.transform = "translate(" + t.offsetLeft + "px," + t.offsetTop + "px)";
    if (list.scrollWidth > list.clientWidth) list.scrollTo({ left: t.offsetLeft - 16, behavior: "smooth" });
  }

  // ---------- Harga per kategori: tab dengan sorotan meluncur ----------
  // Berganti otomatis tiap 7 detik selama terlihat (jeda saat kursor di atasnya);
  // berhenti total begitu pengunjung memilih tab sendiri.
  document.querySelectorAll("[data-pt]").forEach(function (pt, n) {
    var list = pt.querySelector(".pt-tabs");
    var tabs = Array.prototype.slice.call(pt.querySelectorAll(".pt-tab"));
    var panels = pt.querySelectorAll(".pt-panel");
    var cur = 0, visible = false, hover = false;
    pt.classList.add("pt-js");

    function show(i, focus) {
      cur = i;
      tabs.forEach(function (t, k) {
        var on = k === i;
        t.classList.toggle("is-active", on);
        t.setAttribute("aria-selected", on ? "true" : "false");
        t.tabIndex = on ? 0 : -1;
        panels[k].hidden = !on;
        panels[k].classList.toggle("is-active", on);
      });
      glide(list, tabs[i]);
      if (focus) tabs[i].focus();
    }
    function pick(i, focus) {
      pt.classList.remove("pt-auto");
      show((i + tabs.length) % tabs.length, focus);
    }
    function sync() { pt.classList.toggle("pt-paused", !visible || hover); }

    tabs.forEach(function (t, i) {
      var id = "pt" + n + "-" + i;
      t.id = id + "-tab";
      t.setAttribute("aria-controls", id);
      panels[i].id = id;
      panels[i].setAttribute("aria-labelledby", t.id);
      t.addEventListener("click", function () { pick(i, false); });
      t.querySelector(".pt-prog").addEventListener("animationend", function () {
        if (pt.classList.contains("pt-auto")) show((cur + 1) % tabs.length);
      });
    });
    list.addEventListener("keydown", function (e) {
      var step = { ArrowDown: 1, ArrowRight: 1, ArrowUp: -1, ArrowLeft: -1 }[e.key];
      if (e.key === "Home") step = -cur;
      if (e.key === "End") step = tabs.length - 1 - cur;
      if (step === undefined) return;
      e.preventDefault();
      pick(cur + step, true);
    });
    window.addEventListener("resize", function () { show(cur); });
    if (document.fonts) document.fonts.ready.then(function () { show(cur); });
    show(0);

    if (!reduceMotion && tabs.length > 1 && "IntersectionObserver" in window) {
      pt.classList.add("pt-auto");
      sync();
      new IntersectionObserver(function (en) { visible = en[0].isIntersecting; sync(); }, { threshold: 0.35 }).observe(pt);
      pt.addEventListener("mouseenter", function () { hover = true; sync(); });
      pt.addEventListener("mouseleave", function () { hover = false; sync(); });
    }
  });

  // ---------- Blog: tab kategori di kiri ----------
  // [data-blog-tabs] = semua artikel sudah termuat, jadi kategori disaring di browser
  // tanpa memuat ulang. Tanpa atribut itu tab berupa tautan biasa (disaring server).
  document.querySelectorAll(".blog-cats").forEach(function (list) {
    var tabs = Array.prototype.slice.call(list.querySelectorAll(".pt-tab"));
    var active = function () { return list.querySelector(".pt-tab.is-active") || tabs[0]; };
    var place = function () { glide(list, active()); };
    place();
    window.addEventListener("resize", place);
    if (document.fonts) document.fonts.ready.then(place);
    if (!list.hasAttribute("data-blog-tabs")) return;

    var cards = Array.prototype.slice.call(document.querySelectorAll(".blog-grid > .post-tile"));
    var empty = document.querySelector("[data-blog-empty]");
    list.addEventListener("click", function (e) {
      var t = e.target.closest(".pt-tab");
      if (!t || e.metaKey || e.ctrlKey || e.shiftKey) return;
      e.preventDefault();
      var slug = t.getAttribute("data-cat"), n = 0;
      tabs.forEach(function (x) {
        var on = x === t;
        x.classList.toggle("is-active", on);
        if (on) x.setAttribute("aria-current", "page"); else x.removeAttribute("aria-current");
      });
      place();
      cards.forEach(function (card) {
        var on = !slug || card.getAttribute("data-cat") === slug;
        card.hidden = !on;
        card.classList.remove("blog-in");
        if (!on) return;
        card.style.animationDelay = Math.min(n++, 8) * 0.06 + "s";
        void card.offsetWidth;
        card.classList.add("blog-in");
      });
      empty.hidden = n > 0;
      history.replaceState(null, "", t.getAttribute("href"));
    });
  });

  // ---------- Popup WhatsApp: penawaran proyek (#hubungi) & layanan pelanggan (#konsultasi) ----------
  // Dibuka oleh [data-wa-open="<id>"] dan tautan #hubungi. Tombol di blok harga per
  // kategori memilih kategorinya otomatis. Isian disusun jadi pesan WhatsApp.
  if (!document.body.classList.contains("pb-editing")) {
    document.addEventListener("click", function (e) {
      var a = e.target.closest && e.target.closest('[data-wa-open], a[href="#hubungi"]');
      if (!a) return;
      var dlg = document.getElementById(a.getAttribute("data-wa-open") || "hubungi");
      if (!dlg || typeof dlg.showModal !== "function") return;
      e.preventDefault();
      var panel = a.closest(".pt-panel");
      var tab = panel && document.getElementById(panel.getAttribute("aria-labelledby"));
      var pick = tab ? tab.querySelector("strong").textContent.trim() : "";
      dlg.querySelectorAll('input[name="kategori"]').forEach(function (r) { r.checked = !!pick && r.value === pick; });
      dlg.showModal();
      dlg.querySelector('input[name="nama"]').focus();
    });

    document.querySelectorAll("form[data-offer]").forEach(function (form) {
      var dlg = form.closest("dialog");
      dlg.querySelector("[data-offer-close]").addEventListener("click", function () { dlg.close(); });
      dlg.addEventListener("click", function (e) { if (e.target === dlg) dlg.close(); });
      form.addEventListener("submit", function (e) {
        e.preventDefault();
        var v = function (n) { var el = form.elements[n]; return el ? (el.value || "").trim() : ""; };
        var attr = function (n) { return form.getAttribute("data-" + n); };
        var usaha = v("usaha");
        var msg = "Halo " + attr("site") + ", saya " + v("nama") + (usaha ? " dari " + usaha : "") + ".\n\n" +
          attr("intro") + "\n" +
          "• " + attr("topic") + ": " + v("kategori") + "\n" +
          "• " + attr("detail") + ": " + (v("text") || "-") + "\n\nTerima kasih.";
        window.open("https://wa.me/" + attr("offer") + "?text=" + encodeURIComponent(msg), "_blank", "noopener");
        dlg.close();
        form.reset();
      });
    });
  }

  // ---------- Tombol kembali ke atas ----------
  var toTopBtn = document.querySelector("[data-totop]");
  if (toTopBtn) {
    var tickTop = false;
    var showTop = function () {
      tickTop = false;
      toTopBtn.hidden = window.scrollY < 700;
    };
    window.addEventListener("scroll", function () {
      if (!tickTop) { tickTop = true; window.requestAnimationFrame(showTop); }
    }, { passive: true });
    toTopBtn.addEventListener("click", function () {
      window.scrollTo({ top: 0, behavior: reduceMotion ? "auto" : "smooth" });
    });
    showTop();
  }

  // ---------- Tooltip grafik ([data-tip]: baris pertama = judul) ----------
  var tipEl = null;
  var showTip = function (el, x, y) {
    if (!tipEl) {
      tipEl = document.createElement("div");
      tipEl.className = "chart-tip";
      tipEl.setAttribute("role", "status");
      document.body.appendChild(tipEl);
    }
    tipEl.textContent = el.getAttribute("data-tip");
    tipEl.hidden = false;
    var w = tipEl.offsetWidth, h = tipEl.offsetHeight;
    var left = Math.min(Math.max(8, x - w / 2), window.innerWidth - w - 8);
    var top = y - h - 12 < 8 ? y + 16 : y - h - 12;
    tipEl.style.left = left + "px";
    tipEl.style.top = top + "px";
  };
  var hideTip = function () { if (tipEl) tipEl.hidden = true; };
  document.addEventListener("mousemove", function (e) {
    var el = e.target.closest && e.target.closest("[data-tip]");
    if (el) showTip(el, e.clientX, e.clientY); else hideTip();
  });
  document.addEventListener("focusin", function (e) {
    var el = e.target.closest && e.target.closest("[data-tip]");
    if (!el) return hideTip();
    var r = el.getBoundingClientRect();
    showTip(el, r.left + r.width / 2, r.top);
  });
  document.addEventListener("focusout", hideTip);
  window.addEventListener("scroll", hideTip, { passive: true });

  // ---------- Tombol salin ([data-copy]="#id") ----------
  document.addEventListener("click", function (e) {
    var b = e.target.closest && e.target.closest("[data-copy]");
    if (!b) return;
    var el = document.querySelector(b.getAttribute("data-copy"));
    if (!el || !navigator.clipboard) return;
    navigator.clipboard.writeText(el.textContent.trim()).then(function () {
      var old = b.innerHTML;
      b.innerHTML = '<i class="fa-solid fa-check"></i> Tersalin';
      setTimeout(function () { b.innerHTML = old; }, 1600);
    });
  });

  // ---------- Slider ----------
  function slidesOf(slider) {
    return Array.prototype.slice.call(slider.querySelectorAll(".slides > .slide"));
  }
  function slideIndex(slider) {
    var track = slider.querySelector(".slides");
    var s = slidesOf(slider), best = 0, bestD = Infinity;
    s.forEach(function (el, i) {
      var d = Math.abs(el.offsetLeft - track.scrollLeft);
      if (d < bestD) { bestD = d; best = i; }
    });
    return best;
  }
  function renderDots(slider) {
    var s = slidesOf(slider);
    var dots = slider.querySelector("[data-slider-dots]");
    slider.classList.toggle("is-single", s.length <= 1);
    if (!dots) return;
    if (dots.children.length !== s.length) {
      dots.innerHTML = "";
      s.forEach(function (_, i) {
        var b = document.createElement("button");
        b.type = "button";
        b.setAttribute("data-dot", String(i));
        b.setAttribute("aria-label", "Slide " + (i + 1));
        dots.appendChild(b);
      });
    }
    var cur = slideIndex(slider);
    Array.prototype.forEach.call(dots.children, function (b, i) {
      b.classList.toggle("active", i === cur);
      b.setAttribute("aria-current", i === cur ? "true" : "false");
    });
  }
  function goTo(slider, i) {
    var s = slidesOf(slider);
    if (!s.length) return;
    i = (i + s.length) % s.length;
    slider.querySelector(".slides").scrollTo({ left: s[i].offsetLeft, behavior: "smooth" });
  }
  document.addEventListener("click", function (e) {
    var t = e.target.closest && e.target.closest("[data-slide-prev], [data-slide-next], [data-dot]");
    if (!t) return;
    var slider = t.closest("[data-slider]");
    if (!slider) return;
    e.preventDefault();
    if (t.hasAttribute("data-dot")) goTo(slider, parseInt(t.getAttribute("data-dot"), 10));
    else goTo(slider, slideIndex(slider) + (t.hasAttribute("data-slide-next") ? 1 : -1));
  });
  // Titik mengikuti posisi geser (termasuk swipe di layar sentuh).
  var dotTick = null;
  document.addEventListener("scroll", function (e) {
    var el = e.target;
    if (!(el instanceof Element) || !el.classList.contains("slides")) return;
    if (dotTick) cancelAnimationFrame(dotTick);
    dotTick = requestAnimationFrame(function () { renderDots(el.closest("[data-slider]")); });
  }, true);
  var editing = document.body.classList.contains("pb-editing");
  var reduceMotion = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  document.querySelectorAll("[data-slider]").forEach(function (slider) {
    renderDots(slider);
    var secs = parseInt(slider.getAttribute("data-interval"), 10) || 0;
    if (editing || reduceMotion || secs <= 0 || slidesOf(slider).length <= 1) return;
    var paused = false;
    slider.addEventListener("mouseenter", function () { paused = true; });
    slider.addEventListener("mouseleave", function () { paused = false; });
    slider.addEventListener("focusin", function () { paused = true; });
    slider.addEventListener("focusout", function () { paused = false; });
    setInterval(function () {
      if (!paused && !document.hidden) goTo(slider, slideIndex(slider) + 1);
    }, secs * 1000);
  });
  // Di mode edit: perbarui titik setelah slide ditambah/dihapus.
  if (editing) {
    document.addEventListener("click", function (e) {
      var slider = e.target.closest && e.target.closest("[data-slider]");
      if (slider) setTimeout(function () { renderDots(slider); }, 0);
    });
  }

  // ---------- Status server (dashboard Admin): segarkan tiap 10 detik ----------
  var statusCard = document.querySelector("[data-server-status]");
  if (statusCard) {
    var pill = statusCard.querySelector("[data-status-pill]");
    var pillText = statusCard.querySelector("[data-status-text]");
    var setPill = function (cls, text) {
      pill.classList.remove("ok", "warn", "down");
      pill.classList.add(cls);
      pillText.textContent = text;
    };
    var refreshStatus = function () {
      fetch("/admin/status", { credentials: "same-origin", headers: { Accept: "application/json" } })
        .then(function (r) {
          var ct = r.headers.get("content-type") || "";
          if (!r.ok || ct.indexOf("application/json") < 0) throw new Error("status " + r.status);
          return r.json();
        })
        .then(function (d) {
          Object.keys(d).forEach(function (k) {
            statusCard.querySelectorAll('[data-stat="' + k + '"]').forEach(function (el) { el.textContent = d[k]; });
          });
          if (d.db === "Terhubung") setPill("ok", "Online");
          else setPill("warn", "Database bermasalah");
        })
        .catch(function () { setPill("down", "Tidak terhubung"); });
    };
    var statusVisible = function () { return !document.hidden && !statusCard.closest("[hidden]"); };
    setInterval(function () { if (statusVisible()) refreshStatus(); }, 10000);
    document.addEventListener("visibilitychange", function () { if (statusVisible()) refreshStatus(); });
  }

  // ---------- Widget dashboard: sembunyikan / munculkan, disimpan per pengguna ----------
  var widgetForm = document.querySelector("[data-widget-form]");
  if (widgetForm) {
    var submitBtn = widgetForm.querySelector("button[type=submit]");
    if (submitBtn) submitBtn.hidden = true;
    var emptyMsg = document.querySelector("[data-widgets-empty]");
    var widgets = function () { return Array.prototype.slice.call(document.querySelectorAll("[data-widget]")); };
    var syncWidgets = function () {
      var all = widgets();
      widgetForm.querySelectorAll('input[name="show"]').forEach(function (cb) {
        var w = document.querySelector('[data-widget="' + cb.value + '"]');
        if (w) cb.checked = !w.hidden;
      });
      if (emptyMsg) emptyMsg.hidden = all.some(function (w) { return !w.hidden; });
    };
    var saveWidgets = function () {
      var data = new URLSearchParams();
      data.set("_csrf", widgetForm.querySelector("[name=_csrf]").value);
      data.set("hidden", widgets().filter(function (w) { return w.hidden; }).map(function (w) { return w.getAttribute("data-widget"); }).join(","));
      data.set("ajax", "1");
      fetch("/admin/dashboard/widgets", { method: "POST", body: data, credentials: "same-origin" }).catch(function () {});
    };
    var setShown = function (key, shown) {
      var w = document.querySelector('[data-widget="' + key + '"]');
      if (!w) return;
      w.hidden = !shown;
      syncWidgets();
      saveWidgets();
    };
    widgetForm.addEventListener("change", function (e) {
      if (e.target.name === "show") setShown(e.target.value, e.target.checked);
    });
    document.addEventListener("click", function (e) {
      var b = e.target.closest && e.target.closest("[data-widget-hide]");
      if (b) setShown(b.getAttribute("data-widget-hide"), false);
      // Tutup panel bila klik di luar.
      var cfg = widgetForm.closest("details");
      if (cfg && cfg.open && !cfg.contains(e.target)) cfg.open = false;
    });
  }

  // Menu "⋯" di tabel: tutup saat klik di luar atau menu lain dibuka.
  document.addEventListener("click", function (e) {
    document.querySelectorAll("details.row-menu[open]").forEach(function (d) {
      if (!d.contains(e.target)) d.open = false;
    });
  });

  // ---------- Form halaman: alamat otomatis, ikon menu, pratinjau Google ----------
  var pageForm = document.getElementById("page-form");
  if (pageForm) {
    var pq = function (sel) { return pageForm.querySelector(sel); };
    var pTitle = pq("#page-title"), pSlug = pq("#page-slug"), pIcon = pq("#page-icon"), pPrev = pq("#icon-preview");
    var pSt = pq("#seo_title"), pSd = pq("#seo_desc"), pCopy = pq("#copy_from"), pTpl = pq(".tpl-grid");
    var pSite = pageForm.getAttribute("data-site"), pOrigin = pageForm.getAttribute("data-origin");
    var pDesc = document.getElementById("pserp-desc");
    var pageSlugify = function (s) {
      return s.toLowerCase().normalize("NFD").replace(/[\u0300-\u036f]/g, "")
        .replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "").slice(0, 50).replace(/-+$/, "");
    };
    var pageUpdate = function () {
      var t = pTitle.value.trim();
      if (!pSlug.disabled) pSlug.placeholder = pageSlugify(t) || "otomatis-dari-judul";
      document.getElementById("pserp-url").textContent = pOrigin + " › " + (pSlug.value.trim() || pSlug.placeholder);
      document.getElementById("pserp-title").textContent = pSt.value.trim() || ((t || "Judul halaman") + " · " + pSite);
      pDesc.textContent = pSd.value.trim() || pDesc.getAttribute("data-default") || "Deskripsi halaman akan tampil di sini.";
      pageForm.querySelectorAll("[data-count-for]").forEach(function (sp) {
        var el = document.getElementById(sp.getAttribute("data-count-for")), mx = +el.getAttribute("data-max"), n = el.value.length;
        sp.textContent = n + "/" + mx;
        sp.className = n > mx ? "over" : "";
      });
    };
    var setIcon = function (cls) {
      var i = document.createElement("i");
      i.className = cls.trim() || "fa-regular fa-file-lines";
      i.setAttribute("aria-hidden", "true");
      pPrev.replaceChildren(i);
      pageForm.querySelectorAll(".icon-pick").forEach(function (b) {
        b.classList.toggle("active", b.getAttribute("data-icon") === cls.trim());
      });
    };
    [pTitle, pSlug, pSt, pSd].forEach(function (el) { el.addEventListener("input", pageUpdate); });
    pIcon.addEventListener("input", function () { setIcon(pIcon.value); });
    pageForm.addEventListener("click", function (e) {
      var b = e.target.closest(".icon-pick");
      if (!b) return;
      pIcon.value = b.getAttribute("data-icon");
      setIcon(pIcon.value);
    });
    // Saat menyalin halaman lain, pilihan templat tidak dipakai.
    if (pCopy && pTpl) {
      var syncCopy = function () { pTpl.classList.toggle("tpl-off", pCopy.value !== ""); };
      pCopy.addEventListener("change", syncCopy);
      syncCopy();
    }
    pageUpdate();
  }

  var body = document.getElementById("body");
  if (!body) return;

  var form = document.getElementById("post-form");
  var preview = document.getElementById("preview");
  var wordcount = document.getElementById("wordcount");
  var title = document.getElementById("title");
  var slug = document.getElementById("slug");

  function updateCount() {
    // Sama dengan countWords di CMS/Seo.hs: hanya kata yang berisi huruf/angka.
    var words = body.value.split(/\s+/).filter(function (w) { return /[\p{L}\p{N}]/u.test(w); }).length;
    wordcount.textContent = words + " kata · ±" + Math.max(1, Math.round(words / 200)) + " menit baca";
  }
  body.addEventListener("input", updateCount);
  updateCount();

  // Placeholder slug mengikuti judul.
  function slugify(s) {
    return s.toLowerCase().normalize("NFD").replace(/[̀-ͯ]/g, "")
      .replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "").slice(0, 80);
  }
  title.addEventListener("input", function () {
    slug.placeholder = slugify(title.value) || "otomatis-dari-judul";
  });

  // ---------- SEO artikel: pratinjau Google + skor langsung ----------
  // Aturan sama dengan analyze di CMS/Seo.hs.
  var seoPanel = document.getElementById("seo-panel");
  if (seoPanel) {
    var $ = function (id) { return document.getElementById(id); };
    var excerptEl = $("excerpt"), kwEl = $("seo_keyword"), stEl = $("seo_title"), sdEl = $("seo_desc");
    var scoreCard = $("seo-score");
    var blank = function (s) { return !s || !s.trim(); };
    var plainExcerpt = function (n, s) {
      var p = s.replace(/[#*_`>\[\]]/g, "").split(/\s+/).filter(Boolean).join(" ");
      return p.length > n ? p.slice(0, n).replace(/\s+$/, "") + "…" : p;
    };
    var countWords = function (s) { return s.split(/\s+/).filter(function (w) { return /[\p{L}\p{N}]/u.test(w); }).length; };
    var images = function (s) {
      var out = [], re = /!\[([^\]\n]*)\]\(\s*([^)\s]*)[^)]*\)/g, m;
      while ((m = re.exec(s))) out.push([m[1].trim(), m[2]]);
      return out;
    };
    var links = function (s) {
      var imgs = images(s).map(function (x) { return x[1]; });
      var out = [], re = /\]\(\s*([^)\s]*)[^)]*\)/g, m;
      while ((m = re.exec(s))) if (imgs.indexOf(m[1]) < 0) out.push(m[1]);
      return out;
    };
    var firstParagraph = function (s) {
      var ps = s.replace(/\r/g, "").split("\n\n").filter(function (p) {
        p = p.trim();
        return p && p[0] !== "#" && p.indexOf("![") !== 0 && p.indexOf("```") !== 0;
      });
      return ps[0] || "";
    };
    var occurrences = function (kw, s) {
      if (!kw) return 0;
      var n = 0, i = 0;
      while ((i = s.indexOf(kw, i)) >= 0) { n++; i += kw.length; }
      return n;
    };
    var pct = function (d) { return (Math.round(d * 10) / 10).toString().replace(".", ",") + "%"; };
    var effTitle = function () { return blank(stEl.value) ? title.value : stEl.value.trim(); };
    var effDesc = function () {
      if (!blank(sdEl.value)) return sdEl.value.trim();
      if (!blank(excerptEl.value)) return excerptEl.value.trim();
      return plainExcerpt(160, body.value);
    };
    var seoAnalyze = function () {
      var kw = kwEl.value.trim().toLowerCase(), hasKw = !!kw;
      var t = effTitle(), d = effDesc(), b = body.value;
      var sl = slug.value.trim() || slugify(title.value);
      var wc = countWords(b);
      var dens = wc ? occurrences(kw, b.toLowerCase()) * 100 / wc : 0;
      var C = function (lv, text) { return [lv, text]; };
      var kwC = function (ok, good, bad) { return !hasKw ? C("bad", bad) : ok ? C("good", good) : C("bad", bad); };
      var tl = t.length, dl = d.length;
      var imgs = images(b);
      var out = [
        hasKw ? C("good", "Kata kunci utama: “" + kwEl.value.trim() + "”") : C("bad", "Tentukan kata kunci utama artikel ini"),
        kwC(t.toLowerCase().indexOf(kw) >= 0, "Kata kunci ada di judul SEO", "Masukkan kata kunci ke judul SEO"),
        kwC(d.toLowerCase().indexOf(kw) >= 0, "Kata kunci ada di deskripsi meta", "Masukkan kata kunci ke deskripsi meta"),
        kwC(sl.indexOf(slugify(kw)) >= 0, "Kata kunci ada di URL (slug)", "Masukkan kata kunci ke URL (slug)"),
        kwC(firstParagraph(b).toLowerCase().indexOf(kw) >= 0, "Kata kunci muncul di paragraf pertama", "Sebutkan kata kunci di paragraf pertama"),
        !hasKw ? C("bad", "Kepadatan kata kunci belum bisa dihitung")
          : dens < 0.5 ? C("warn", "Kata kunci jarang dipakai (" + pct(dens) + "); idealnya 0,5–2,5%")
          : dens > 2.5 ? C("warn", "Kata kunci terlalu sering (" + pct(dens) + "); idealnya 0,5–2,5%")
          : C("good", "Kepadatan kata kunci pas (" + pct(dens) + ")"),
        tl >= 30 && tl <= 60 ? C("good", "Panjang judul SEO pas (" + tl + " karakter)")
          : tl === 0 ? C("bad", "Judul masih kosong")
          : tl < 30 ? C("warn", "Judul SEO terlalu pendek (" + tl + "/30–60 karakter)")
          : C("warn", "Judul SEO terlalu panjang, akan terpotong di Google (" + tl + "/60)"),
        blank(sdEl.value) ? C("bad", "Tulis deskripsi meta sendiri (saat ini diambil otomatis)")
          : dl >= 120 && dl <= 160 ? C("good", "Panjang deskripsi meta pas (" + dl + " karakter)")
          : dl < 120 ? C("warn", "Deskripsi meta terlalu pendek (" + dl + "/120–160 karakter)")
          : C("warn", "Deskripsi meta terlalu panjang (" + dl + "/160 karakter)"),
        wc >= 300 ? C("good", "Panjang artikel cukup (" + wc + " kata)")
          : wc >= 150 ? C("warn", "Artikel agak pendek (" + wc + " kata); targetkan 300+")
          : C("bad", "Artikel terlalu pendek (" + wc + " kata); targetkan 300+"),
        /^\s*#{2,3} /m.test(b) ? C("good", "Ada subjudul (## …) untuk memecah isi") : C("warn", "Tambahkan subjudul (## …) agar mudah dibaca"),
        !imgs.length ? C("warn", "Tambahkan minimal satu gambar")
          : imgs.some(function (x) { return !x[0]; }) ? C("bad", "Ada gambar tanpa teks alternatif (alt)")
          : C("good", "Semua gambar punya teks alternatif"),
        links(b).some(function (u) { return u[0] === "/" && u[1] !== "/"; })
          ? C("good", "Ada tautan ke halaman lain di situs ini") : C("warn", "Tambahkan tautan ke artikel/halaman lain di situs ini")
      ];
      return out;
    };
    var LV = { good: ["lv-good", "fa-circle-check"], warn: ["lv-warn", "fa-circle-exclamation"], bad: ["lv-bad", "fa-circle-xmark"] };
    var renderSeo = function () {
      // Pratinjau Google
      $("serp-title").textContent = effTitle() || "Judul artikel";
      $("serp-desc").textContent = effDesc() || "Deskripsi artikel akan tampil di sini.";
      $("serp-url").textContent = seoPanel.getAttribute("data-origin") + " › post › " + (slug.value.trim() || slugify(title.value) || "…");
      seoPanel.querySelectorAll("[data-count-for]").forEach(function (sp) {
        var el = $(sp.getAttribute("data-count-for")), mx = +el.getAttribute("data-max"), n = el.value.length;
        sp.textContent = n + "/" + mx;
        sp.className = n > mx ? "over" : "";
      });
      // Skor
      var checks = seoAnalyze();
      var score = Math.round(100 * checks.reduce(function (a, c) { return a + (c[0] === "good" ? 1 : c[0] === "warn" ? 0.5 : 0); }, 0) / checks.length);
      var lv = score >= 80 ? "good" : score >= 50 ? "warn" : "bad";
      var ring = scoreCard.querySelector(".score-ring");
      ring.className = "score-ring seo-ring " + LV[lv][0];
      ring.setAttribute("aria-label", "Skor SEO " + score + " dari 100");
      ring.querySelector(".ring-fg").setAttribute("stroke-dasharray", score + " 100");
      ring.querySelector(".score-num").textContent = score;
      var lvEl = scoreCard.querySelector("[data-seo-level]");
      lvEl.className = "seo-level " + LV[lv][0];
      lvEl.textContent = { good: "Baik", warn: "Cukup", bad: "Perlu perbaikan" }[lv];
      var ul = scoreCard.querySelector("[data-seo-checks]");
      ul.textContent = "";
      checks.forEach(function (c) {
        var li = document.createElement("li");
        li.className = LV[c[0]][0];
        li.innerHTML = '<i class="fa-solid ' + LV[c[0]][1] + '" aria-hidden="true"></i>';
        var sp = document.createElement("span");
        sp.textContent = c[1];
        li.appendChild(sp);
        ul.appendChild(li);
      });
    };
    var seoTimer = null;
    var queueSeo = function () { clearTimeout(seoTimer); seoTimer = setTimeout(renderSeo, 200); };
    [title, slug, body, excerptEl, kwEl, stEl, sdEl].forEach(function (el) { el.addEventListener("input", queueSeo); });
    renderSeo();
  }

  // Toolbar Markdown.
  function wrap(before, after, fallback) {
    var s = body.selectionStart, e = body.selectionEnd;
    var sel = body.value.slice(s, e) || fallback;
    body.setRangeText(before + sel + after, s, e, "end");
    body.selectionStart = s + before.length;
    body.selectionEnd = s + before.length + sel.length;
  }
  function prefixLines(prefix) {
    var s = body.selectionStart, e = body.selectionEnd, v = body.value;
    var start = v.lastIndexOf("\n", s - 1) + 1;
    var block = v.slice(start, e) || "teks";
    var n = 0;
    var out = block.split("\n").map(function (line) {
      n += 1;
      return (typeof prefix === "function" ? prefix(n) : prefix) + line;
    }).join("\n");
    body.setRangeText(out, start, e, "select");
  }
  var actions = {
    h2: function () { prefixLines("## "); },
    bold: function () { wrap("**", "**", "teks tebal"); },
    italic: function () { wrap("*", "*", "teks miring"); },
    link: function () {
      var url = window.prompt("URL tautan:", "https://");
      if (url) wrap("[", "](" + url + ")", "teks tautan");
    },
    image: function () {
      var url = window.prompt("URL gambar:", "https://");
      if (url) wrap("![", "](" + url + ")", "deskripsi gambar");
    },
    ul: function () { prefixLines("- "); },
    ol: function () { prefixLines(function (n) { return n + ". "; }); },
    quote: function () { prefixLines("> "); },
    code: function () {
      var sel = body.value.slice(body.selectionStart, body.selectionEnd);
      if (sel.indexOf("\n") >= 0) wrap("```\n", "\n```", "");
      else wrap("`", "`", "kode");
    }
  };
  document.querySelectorAll("[data-md]").forEach(function (btn) {
    btn.addEventListener("click", function () {
      var fn = actions[btn.getAttribute("data-md")];
      if (fn) { body.focus(); fn(); body.dispatchEvent(new Event("input")); }
    });
  });

  // Tab tulis / pratinjau. Pratinjau dirender server (renderer yang sama dengan situs).
  var tabs = document.querySelectorAll("[data-tab]");
  tabs.forEach(function (tab) {
    tab.addEventListener("click", function () {
      tabs.forEach(function (t) { t.classList.toggle("active", t === tab); });
      if (tab.getAttribute("data-tab") === "preview") {
        var data = new URLSearchParams();
        data.set("_csrf", form.querySelector("[name=_csrf]").value);
        data.set("body", body.value);
        preview.innerHTML = "<p class='muted'>Memuat pratinjau…</p>";
        body.hidden = true;
        preview.hidden = false;
        fetch("/admin/preview", { method: "POST", body: data, credentials: "same-origin" })
          .then(function (r) { return r.ok ? r.text() : Promise.reject(r.status); })
          .then(function (html) {
            preview.innerHTML = html || "<p class='muted'>Belum ada isi.</p>";
          })
          .catch(function () { preview.innerHTML = "<p class='muted'>Gagal memuat pratinjau.</p>"; });
      } else {
        preview.hidden = true;
        body.hidden = false;
        body.focus();
      }
    });
  });

  // Ctrl/Cmd+S menyimpan, dan peringatan bila meninggalkan perubahan belum disimpan.
  var dirty = false;
  form.addEventListener("input", function () { dirty = true; });
  form.addEventListener("submit", function () { dirty = false; });
  window.addEventListener("beforeunload", function (e) {
    if (dirty) { e.preventDefault(); e.returnValue = ""; }
  });
  document.addEventListener("keydown", function (e) {
    if ((e.ctrlKey || e.metaKey) && e.key === "s") {
      e.preventDefault();
      dirty = false;
      form.requestSubmit();
    }
  });
})();
