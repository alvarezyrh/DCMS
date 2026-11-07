module Main (main) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setGracefulShutdownTimeout, setHost, setInstallShutdownHandler, setPort)
import System.Posix.Signals (Handler (Catch), installHandler, sigTERM)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Data.Time.Clock (getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import Control.Exception (bracket_)
import CMS.Auth (hashPassword)
import CMS.DB (deleteUserSessions, findUserByLogin, updatePassword)
import System.IO (BufferMode (..), hFlush, hIsTerminalDevice, hPutStrLn, hSetBuffering, hSetEcho, hSetEncoding, stderr, stdin, stdout, utf8)
import Text.Read (readMaybe)

import CMS.App (mkApp, seedIfEmpty)
import CMS.RateLimit (newLimiter)
import Control.Monad (mfilter, void, when)
import CMS.DB (openDb)
import CMS.Types (Env (..), User (..))
import Control.Concurrent.MVar (withMVar)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["reset-password", uname] -> resetPassword (T.pack uname)
    [] -> serve
    _ -> do
      hPutStrLn stderr "Pemakaian: DCMS                         (jalankan server)"
      hPutStrLn stderr "           DCMS reset-password <username> (ganti password; baca CMS_NEW_PASSWORD atau ketik)"
      exitFailure

-- | Ganti password pengguna dari baris perintah (mis. admin lupa password):
--   docker compose exec -u 10001 dcms /app/DCMS reset-password admin
-- Password dibaca dari CMS_NEW_PASSWORD, atau diketik (tidak tampil di layar).
-- Semua sesi pengguna itu dihapus sehingga harus login ulang.
resetPassword :: T.Text -> IO ()
resetPassword uname = do
  hSetEncoding stdout utf8
  dbPath <- fromMaybe "cms.db" <$> lookupEnv "CMS_DB"
  fromEnv <- mfilter (not . null) <$> lookupEnv "CMS_NEW_PASSWORD"
  pw <- case fromEnv of
    Just p -> pure p
    Nothing -> do
      putStr "Password baru: " >> hFlush stdout
      p1 <- withoutEcho getLine
      putStr "\nUlangi password baru: " >> hFlush stdout
      p2 <- withoutEcho getLine
      putStrLn ""
      if p1 /= p2 then hPutStrLn stderr "GALAT: kedua password tidak sama." >> exitFailure else pure p1
  when (length pw < 8) $ hPutStrLn stderr "GALAT: password minimal 8 karakter." >> exitFailure
  db <- openDb dbPath
  found <- withMVar db $ \c -> do
    mu <- findUserByLogin c uname
    case mu of
      Nothing -> pure False
      Just u -> do
        h <- hashPassword (T.pack pw)
        updatePassword c (userId u) h
        deleteUserSessions c (userId u)
        pure True
  if found
    then putStrLn ("Password untuk \"" <> T.unpack uname <> "\" sudah diganti. Silakan login dengan password baru.")
    else hPutStrLn stderr ("GALAT: pengguna \"" <> T.unpack uname <> "\" tidak ditemukan.") >> exitFailure
  where
    withoutEcho act = do
      tty <- hIsTerminalDevice stdin
      if tty then bracket_ (hSetEcho stdin False) (hSetEcho stdin True) act else act

serve :: IO ()
serve = do
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
