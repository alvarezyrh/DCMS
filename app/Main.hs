module Main (main) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setGracefulShutdownTimeout, setHost, setInstallShutdownHandler, setPort)
import System.Posix.Signals (Handler (Catch), installHandler, sigTERM)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Data.Time.Clock (getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.IO (BufferMode (..), hSetBuffering, hSetEncoding, stderr, stdout, utf8)
import Text.Read (readMaybe)

import CMS.App (mkApp, seedIfEmpty)
import CMS.RateLimit (newLimiter)
import Control.Monad (mfilter, void)
import CMS.DB (openDb)
import CMS.Types (Env (..))
import Control.Concurrent.MVar (withMVar)

main :: IO ()
main = do
  -- Di kontainer stdout bukan terminal: tanpa ini log tertahan di buffer. Locale
  -- kontainer biasanya ASCII; paksa UTF-8 agar mencetak teks seperti "…" tidak error.
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  port <- fromMaybe 3000 . (>>= readMaybe) <$> lookupEnv "CMS_PORT"
  dbPath <- fromMaybe "cms.db" <$> lookupEnv "CMS_DB"
  staticDir <- fromMaybe "static" <$> lookupEnv "CMS_STATIC"
  assetDir <- fromMaybe "src/asset/img" <$> lookupEnv "CMS_ASSETS"
  createDirectoryIfMissing True assetDir
  adminPw <- fmap T.pack . mfilter (not . null) <$> lookupEnv "CMS_ADMIN_PASSWORD"
  started <- getCurrentTime
  limiter <- newLimiter
  -- Produksi: CMS_SITE_URL=https://domain-anda (alamat resmi, juga mengaktifkan cookie
  -- Secure + HSTS bila https) dan CMS_TRUST_PROXY=1 bila berjalan di belakang reverse proxy.
  siteUrl <- fmap (T.dropWhileEnd (== '/') . T.strip . T.pack) . mfilter (not . null) <$> lookupEnv "CMS_SITE_URL"
  secureEnv <- lookupEnv "CMS_COOKIE_SECURE"
  trustProxy <- (== Just "1") <$> lookupEnv "CMS_TRUST_PROXY"
  let secure = maybe (maybe False ("https://" `T.isPrefixOf`) siteUrl) (== "1") secureEnv
  db <- openDb dbPath
  withMVar db (\c -> seedIfEmpty secure c adminPw)
  putStrLn ("[config] alamat situs: " <> maybe "(dari header Host)" T.unpack siteUrl <> ", cookie Secure: " <> show secure <> ", percaya proxy: " <> show trustProxy)
  putStrLn ("DCMS berjalan di port " <> show port)
  let app = logStdout (mkApp (Env db staticDir assetDir dbPath started limiter siteUrl secure trustProxy))
      -- "*6" = IPv6 + IPv4 (dual-stack). SIGTERM (podman stop / systemctl stop): berhenti menerima
      -- koneksi baru, selesaikan yang sedang berjalan (maks. 5 detik, di bawah batas 10 detik podman stop), lalu keluar.
      -- Tanpa ini, proses sebagai PID 1 di kontainer mengabaikan SIGTERM.
      settings =
        setPort port
          . setHost "*6"
          . setGracefulShutdownTimeout (Just 5)
          . setInstallShutdownHandler (\closeSocket -> void (installHandler sigTERM (Catch (putStrLn "Menerima SIGTERM, berhenti…" >> closeSocket)) Nothing))
          $ defaultSettings
  runSettings settings app
  putStrLn "DCMS berhenti."
