// Home versi "liar" (hanya dimuat di Home): prompt terminal mengetik, nama raksasa &
// lantai grid di hero, lampu sorot, orbit ikon layanan di foto, pita berjalan yang ikut
// kecepatan gulir, kartu miring 3D, angka terisi, percikan CTA, kursor, dan ledakan
// partikel (klik foto/nama, atau ketik nama depan). Efek judul h1/h2 ada di app.js. Semua isinya diambil dari halaman, elemen hiasan aria-hidden,
// dan gerak dimatikan bila pengunjung memilih gerak minimal.
(function () {
  "use strict";
  var body = document.body;
  if (!body.classList.contains("page-home")) return;

  var reduce = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  var fine = window.matchMedia("(hover: hover) and (pointer: fine)").matches;
  var palette = ["var(--primary)", "#8b5cf6", "#38bdf8", "#f472b6"];
  var hero = document.querySelector(".pb-hero");

  function make(tag, cls, parent) {
    var e = document.createElement(tag);
    if (cls) e.className = cls;
    if (parent) parent.appendChild(e);
    return e;
  }
  function deco(e) { e.setAttribute("aria-hidden", "true"); return e; }
  function texts(sel) {
    return Array.prototype.map.call(document.querySelectorAll(sel), function (n) { return n.textContent.trim(); })
      .filter(Boolean);
  }
  function clamp(v, a, b) { return Math.min(b, Math.max(a, v)); }

  // ---------- Hero ----------
  // Judul hero sudah dipecah per huruf oleh app.js; teks aslinya ada di span.sr-only.
  var heroTitle = hero && hero.querySelector("h1");
  var nameEl = heroTitle && heroTitle.querySelector(".sr-only .hl, .hl");
  var name = nameEl ? nameEl.textContent.trim() : "";
  function wave() { if (heroTitle) heroTitle.dispatchEvent(new Event("hx:wave")); }
  if (heroTitle && !reduce) setInterval(function () { if (!document.hidden) wave(); }, 7000);

  // Prompt terminal: "~/doni $" + teks eyebrow yang diketik ulang.
  var eyebrow = hero && hero.querySelector(".eyebrow");
  if (eyebrow) {
    var first = (name.split(/\s+/)[0] || "home").toLowerCase();
    eyebrow.classList.add("hx-term");
    eyebrow.setAttribute("data-prompt", "~/" + first + " $");
    if (!reduce) {
      var full = eyebrow.textContent;
      var esr = make("span", "sr-only");
      esr.textContent = full;
      var typed = deco(make("span", "hx-type"));
      eyebrow.textContent = "";
      eyebrow.appendChild(esr);
      eyebrow.appendChild(typed);
      deco(make("span", "hx-caret", eyebrow));
      var pos = 0;
      setTimeout(function tick() {
        typed.textContent = full.slice(0, ++pos);
        if (pos < full.length) setTimeout(tick, 38 + Math.random() * 60);
      }, 600);
    } else deco(make("span", "hx-caret", eyebrow));
  }

  if (hero) {
    var floor = deco(make("div", "hx-floor"));
    make("i", "", floor);
    var spot = deco(make("div", "hx-spot"));
    var mark = deco(make("div", "hx-name"));
    mark.textContent = name;
    [spot, mark, floor].forEach(function (e) { hero.insertBefore(e, hero.firstChild); });
  }

  // Orbit ikon layanan (diambil dari kartu layanan) mengelilingi foto.
  var media = hero && hero.querySelector(".hero-media");
  if (media) {
    var icons = Array.prototype.map.call(document.querySelectorAll(".pb-features .pb-ico i"), function (i) { return i.className; })
      .filter(function (c, i, all) { return all.indexOf(c) === i; }).slice(0, 6);
    if (icons.length < 3) icons = ["fa-solid fa-code", "fa-solid fa-server", "fa-solid fa-shield-halved"];
    var orbit = deco(make("div", "hx-orbit", media));
    icons.forEach(function (cls, i) {
      var arm = make("span", "hx-arm", orbit);
      arm.style.setProperty("--a", (360 / icons.length * i) + "deg");
      var chip = make("span", "hx-chip", arm);
      chip.style.setProperty("--c", palette[i % palette.length]);
      make("i", cls, chip);
    });
  }

  // ---------- Pita berjalan: nama + judul layanan & alur kerja ----------
  var tracks = [];
  if (hero) {
    var words = [name].concat(texts(".pb-features .pb-card h3")).filter(Boolean);
    if (words.length > 1) {
      var band = deco(make("div", "hx-marquee"));
      ["hx-belt hx-belt-a", "hx-belt hx-belt-b"].forEach(function (cls, k) {
        var track = make("div", "hx-track", make("div", cls, band));
        var seq = k ? words.slice().reverse() : words;
        for (var half = 0; half < 2; half++) {
          var h = make("div", "hx-half", track);
          for (var rep = 0; rep < 2; rep++) {
            seq.forEach(function (w) {
              make("span", "hx-item", h).textContent = w;
              make("span", "hx-star", h).textContent = "✦";
            });
          }
        }
        if (!reduce && track.animate) {
          var anim = track.animate([{ transform: "translate3d(0,0,0)" }, { transform: "translate3d(-50%,0,0)" }],
            { duration: 26000 + words.join("").length * 220, iterations: Infinity, direction: k ? "reverse" : "normal" });
          tracks.push(anim);
        }
      });
      hero.parentNode.insertBefore(band, hero.nextSibling);
      if (tracks.length && "IntersectionObserver" in window) {
        new IntersectionObserver(function (en) {
          tracks.forEach(function (a) { if (en[0].isIntersecting) a.play(); else a.pause(); });
        }).observe(band);
      }
    }
  }

  // ---------- Percikan di CTA ----------
  var cta = document.querySelector(".cta-box");
  if (cta && !reduce) {
    var sparks = deco(make("div", "hx-sparks", cta));
    for (var s = 0; s < 16; s++) {
      var p = make("i", "", sparks);
      p.style.setProperty("--x", (Math.random() * 100).toFixed(1) + "%");
      p.style.setProperty("--s", (3 + Math.random() * 5).toFixed(1) + "px");
      p.style.setProperty("--d", (4 + Math.random() * 5).toFixed(1) + "s");
      p.style.setProperty("--dl", (-Math.random() * 8).toFixed(1) + "s");
      p.style.setProperty("--dx", ((Math.random() - .5) * 120).toFixed(0) + "px");
    }
  }

  // ---------- Angka alur kerja terisi saat terlihat ----------
  var steps = document.querySelectorAll(".fs-number .pb-card");
  if ("IntersectionObserver" in window && !reduce) {
    var lit = new IntersectionObserver(function (en) {
      en.forEach(function (e) {
        if (!e.isIntersecting) return;
        var i = Array.prototype.indexOf.call(e.target.parentNode.children, e.target);
        setTimeout(function () { e.target.classList.add("hx-lit"); }, i * 160);
        lit.unobserve(e.target);
      });
    }, { threshold: .5 });
    steps.forEach(function (c) { lit.observe(c); });
  }

  // ---------- Gulir: nama raksasa bergeser, garis waktu, kecepatan pita ----------
  var stepGrid = document.querySelector(".fs-number .pb-grid");
  var lastY = window.scrollY, boost = 0, dir = 1, decaying = false;
  function decay() {
    boost *= 0.92;
    var rate = dir * (1 + boost);
    tracks.forEach(function (a) { a.playbackRate = rate; });
    if (boost > 0.02) requestAnimationFrame(decay);
    else { decaying = false; tracks.forEach(function (a) { a.playbackRate = dir; }); }
  }
  var scrollQueued = false;
  window.addEventListener("scroll", function () {
    if (scrollQueued) return;
    scrollQueued = true;
    requestAnimationFrame(function () {
      scrollQueued = false;
      var y = window.scrollY, dy = y - lastY;
      lastY = y;
      if (mark) mark.style.setProperty("--wm", (-y * 0.45) + "px");
      if (stepGrid) {
        var r = stepGrid.getBoundingClientRect(), vh = window.innerHeight;
        stepGrid.style.setProperty("--prog", clamp((vh * 0.9 - r.top) / (r.height + vh * 0.35), 0, 1).toFixed(3));
      }
      if (tracks.length && dy) {
        dir = dy > 0 ? 1 : -1;
        boost = Math.min(boost + Math.abs(dy) * 0.04, 7);
        if (!decaying) { decaying = true; requestAnimationFrame(decay); }
      }
    });
  }, { passive: true });
  window.dispatchEvent(new Event("scroll"));

  // Hero berhenti bergerak saat tidak terlihat.
  if (hero && "IntersectionObserver" in window) {
    new IntersectionObserver(function (en) { hero.classList.toggle("hx-paused", !en[0].isIntersecting); }).observe(hero);
  }

  // ---------- Ledakan partikel ----------
  function burst(x, y, n) {
    if (reduce || !body.animate) return;
    for (var i = 0; i < n; i++) {
      var b = deco(make("span", "hx-bit" + (i % 3 ? "" : " sq"), body));
      b.style.left = x + "px";
      b.style.top = y + "px";
      b.style.background = palette[i % palette.length];
      var a = Math.random() * Math.PI * 2, d = 70 + Math.random() * 150;
      b.animate([
        { transform: "translate(-50%,-50%) scale(1)", opacity: 1 },
        { transform: "translate(calc(-50% + " + (Math.cos(a) * d).toFixed(0) + "px), calc(-50% + " + (Math.sin(a) * d).toFixed(0) +
            "px)) rotate(" + (Math.random() * 720).toFixed(0) + "deg) scale(0)", opacity: 0 }
      ], { duration: 700 + Math.random() * 600, easing: "cubic-bezier(.15,.7,.3,1)" }).onfinish = (function (el) {
        return function () { el.remove(); };
      })(b);
    }
  }
  [media && media.querySelector(".hero-img"), heroTitle].forEach(function (t) {
    if (t) t.addEventListener("click", function (e) { burst(e.clientX, e.clientY, 26); wave(); });
  });
  // Rahasia kecil: ketik nama depan di mana saja (di luar kolom isian).
  var secret = (name.split(/\s+/)[0] || "").toLowerCase(), typedKeys = "";
  if (secret) {
    document.addEventListener("keydown", function (e) {
      if (e.target.closest && e.target.closest("input, textarea, select, [contenteditable]")) return;
      if (e.key.length !== 1) return;
      typedKeys = (typedKeys + e.key.toLowerCase()).slice(-secret.length);
      if (typedKeys !== secret) return;
      typedKeys = "";
      var w = window.innerWidth, h = window.innerHeight;
      [[.5, .4], [.2, .3], [.8, .3], [.3, .7], [.7, .7]].forEach(function (pt, i) {
        setTimeout(function () { burst(w * pt[0], h * pt[1], 30); }, i * 140);
      });
      wave();
    });
  }

  if (reduce || !fine) return;

  // ---------- Hanya mouse: sorot, parallax foto, kartu miring, tombol magnet, kursor ----------
  if (hero) {
    hero.addEventListener("pointermove", function (e) {
      var r = hero.getBoundingClientRect();
      hero.style.setProperty("--sx", (e.clientX - r.left) + "px");
      hero.style.setProperty("--sy", (e.clientY - r.top) + "px");
      if (media) {
        media.style.setProperty("--px", ((e.clientX - r.left) / r.width - .5).toFixed(3));
        media.style.setProperty("--py", ((e.clientY - r.top) / r.height - .5).toFixed(3));
      }
    });
  }

  document.querySelectorAll(".pb-features .pb-card, .post-tile").forEach(function (card) {
    card.classList.add("hx-tilt");
    card.addEventListener("pointermove", function (e) {
      var r = card.getBoundingClientRect(), x = (e.clientX - r.left) / r.width, y = (e.clientY - r.top) / r.height;
      card.style.setProperty("--rx", ((.5 - y) * 12).toFixed(2) + "deg");
      card.style.setProperty("--ry", ((x - .5) * 14).toFixed(2) + "deg");
    });
    card.addEventListener("pointerleave", function () {
      card.style.setProperty("--rx", "0deg");
      card.style.setProperty("--ry", "0deg");
    });
  });

  document.querySelectorAll(".pb-hero .btn, .cta-box .btn").forEach(function (btn) {
    btn.addEventListener("pointermove", function (e) {
      var r = btn.getBoundingClientRect();
      btn.style.translate = ((e.clientX - r.left - r.width / 2) * .3).toFixed(1) + "px " + ((e.clientY - r.top - r.height / 2) * .45).toFixed(1) + "px";
    });
    btn.addEventListener("pointerleave", function () { btn.style.translate = ""; });
  });

  var dot = deco(make("div", "hx-dot", body)), ring = deco(make("div", "hx-ring", body));
  var mx = -100, my = -100, rx = mx, ry = my, following = false;
  function follow() {
    rx += (mx - rx) * 0.2;
    ry += (my - ry) * 0.2;
    ring.style.transform = "translate3d(" + rx.toFixed(1) + "px," + ry.toFixed(1) + "px,0)";
    if (Math.abs(mx - rx) + Math.abs(my - ry) > 0.4) requestAnimationFrame(follow);
    else following = false;
  }
  document.addEventListener("pointermove", function (e) {
    mx = e.clientX;
    my = e.clientY;
    dot.style.transform = "translate3d(" + mx + "px," + my + "px,0)";
    body.classList.add("hx-cur");
    if (!following) { following = true; requestAnimationFrame(follow); }
  }, { passive: true });
  document.addEventListener("pointerover", function (e) {
    ring.classList.toggle("is-link", !!(e.target.closest && e.target.closest("a, button, input, textarea, select, label, [role=button], .hx-split, .hero-img")));
  });
  document.addEventListener("pointerdown", function () { ring.classList.add("is-down"); });
  document.addEventListener("pointerup", function () { ring.classList.remove("is-down"); });
  document.documentElement.addEventListener("pointerleave", function () { body.classList.remove("hx-cur"); });
})();
