# DCMS: image produksi (Podman/Docker). Build dua tahap: kompilasi Haskell, lalu
# image kecil berisi binary + aset statis saja.
#   podman build --format docker -t localhost/dcms:latest .
#   podman run -d -p 3000:3000 -v dcms-data:/data:U -e CMS_ADMIN_PASSWORD=... localhost/dcms:latest
# (--format docker agar HEALTHCHECK ikut tersimpan; format OCI mengabaikannya.)

# --- Tahap 1: build ---------------------------------------------------------------
FROM docker.io/library/haskell:9.10.3-slim-bookworm AS build
RUN apt-get update \
 && apt-get install -y --no-install-recommends zlib1g-dev pkg-config \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /src
RUN cabal update
# Dependensi dulu (versi dikunci di cabal.project.freeze) agar lapisan ini di-cache.
COPY DCMS.cabal cabal.project.freeze ./
RUN cabal build --only-dependencies lib:DCMS -j
COPY app ./app
COPY src ./src
RUN cabal build exe:DCMS -j \
 && mkdir -p /out \
 && cp "$(cabal list-bin DCMS)" /out/DCMS \
 && strip /out/DCMS

# --- Tahap 2: runtime -------------------------------------------------------------
FROM docker.io/library/debian:bookworm-slim
RUN apt-get update \
 && apt-get install -y --no-install-recommends libgmp10 zlib1g ca-certificates curl \
 && rm -rf /var/lib/apt/lists/* \
 && useradd --system --uid 10001 --home-dir /app --shell /usr/sbin/nologin dcms \
 && mkdir -p /data/assets \
 && chown -R dcms:dcms /data
WORKDIR /app
COPY --from=build /out/DCMS /app/DCMS
COPY static /app/static
COPY src/asset/img/placeholder.svg /app/defaults/placeholder.svg
COPY deploy/entrypoint.sh /app/entrypoint.sh
ENV LANG=C.UTF-8 \
    CMS_PORT=3000 \
    CMS_DB=/data/cms.db \
    CMS_ASSETS=/data/assets \
    CMS_STATIC=/app/static
USER dcms
VOLUME ["/data"]
EXPOSE 3000
HEALTHCHECK --interval=30s --timeout=5s --start-period=15s --retries=3 \
  CMD curl -fsS -o /dev/null http://127.0.0.1:3000/robots.txt || exit 1
ENTRYPOINT ["/bin/sh", "/app/entrypoint.sh"]
