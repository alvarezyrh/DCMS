-- | Log keamanan: catat aktivitas penting dan mencurigakan (login gagal, akses ditolak,
-- pemindaian celah, perubahan akun) untuk ditinjau Admin di menu Log.
module CMS.Log
  ( Level (..)
  , Event (..)
  , levelKey
  , levelLabel
  , eventKey
  , eventLabel
  , eventLevel
  , eventByKey
  , logEvent
  , isProbe
  ) where

import Control.Exception (SomeException, try)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (hUserAgent)
import Network.Wai (rawPathInfo, requestHeaders)
import System.IO (hPutStrLn, stderr)

import CMS.DB (insertLog, withDb)
import CMS.Types
import CMS.Web (clientIp)

data Level = Info | Warn | Danger
  deriving (Eq, Enum, Bounded)

levelKey :: Level -> Text
levelKey = \case
  Info -> "info"
  Warn -> "warn"
  Danger -> "danger"

levelLabel :: Level -> Text
levelLabel = \case
  Info -> "Info"
  Warn -> "Peringatan"
  Danger -> "Bahaya"

data Event
  = LoginOk
  | LoginFail
  | LoginBlocked
  | LoginInactive
  | Register
  | CsrfFail
  | Forbidden
  | Probe
  | PasswordChanged
  | PasswordFail
  | UserCreated
  | UserUpdated
  | UserDeleted
  | ApiKeyCreated
  | ApiKeyDeleted
  | ApiDenied
  | ApiLimited
  | ReportExported
  deriving (Eq, Enum, Bounded)

-- | (kunci di database, label, tingkat)
eventInfo :: Event -> (Text, Text, Level)
eventInfo = \case
  LoginOk -> ("login_ok", "Login berhasil", Info)
  LoginFail -> ("login_fail", "Login gagal", Warn)
  LoginBlocked -> ("login_blocked", "Login diblokir sementara", Danger)
  LoginInactive -> ("login_inactive", "Login akun nonaktif", Warn)
  Register -> ("register", "Pendaftaran akun", Info)
  CsrfFail -> ("csrf_fail", "Token formulir tidak valid", Warn)
  Forbidden -> ("forbidden", "Akses dashboard ditolak", Warn)
  Probe -> ("probe", "Pemindaian celah", Danger)
  PasswordChanged -> ("password_changed", "Password diganti", Info)
  PasswordFail -> ("password_fail", "Ganti password gagal", Warn)
  UserCreated -> ("user_created", "Pengguna ditambahkan", Info)
  UserUpdated -> ("user_updated", "Pengguna diubah", Info)
  UserDeleted -> ("user_deleted", "Pengguna dihapus", Warn)
  ApiKeyCreated -> ("api_key_created", "API key dibuat", Info)
  ApiKeyDeleted -> ("api_key_deleted", "API key dihapus", Info)
  ApiDenied -> ("api_denied", "Permintaan API ditolak", Warn)
  ApiLimited -> ("api_limited", "Batas API terlampaui", Warn)
  ReportExported -> ("report_exported", "Laporan diunduh", Info)

eventKey, eventLabel :: Event -> Text
eventKey e = let (k, _, _) = eventInfo e in k
eventLabel e = let (_, l, _) = eventInfo e in l

eventLevel :: Event -> Level
eventLevel e = let (_, _, v) = eventInfo e in v

eventByKey :: Text -> Maybe Event
eventByKey k = find ((== k) . eventKey) [minBound .. maxBound]

-- | Catat satu peristiwa dari request ini. @account@ = akun yang dituju (boleh kosong).
-- Kegagalan menulis log hanya dicetak ke stderr, tidak mengganggu respons.
logEvent :: Ctx -> Event -> Text -> Text -> IO ()
logEvent c ev account detail = do
  r <- try $ withDb (cEnv c) $ \db ->
    insertLog
      db
      ( levelKey (eventLevel ev)
      , eventKey ev
      , clientIp c
      , maybe "" userName (cUser c)
      , T.take 120 account
      , TE.decodeUtf8Lenient (cMethod c)
      , T.take 300 (TE.decodeUtf8Lenient (rawPathInfo (cReq c)))
      , T.take 300 (maybe "" TE.decodeUtf8Lenient (lookup hUserAgent (requestHeaders (cReq c))))
      , T.take 500 detail
      )
  case r of
    Left (e :: SomeException) -> hPutStrLn stderr ("[log] " <> show e)
    Right () -> pure ()

-- | Alamat yang khas dicari pemindai celah (WordPress, phpMyAdmin, berkas rahasia, skrip
-- server). Situs ini tidak punya satu pun, jadi permintaan ke sana patut dicurigai.
isProbe :: [Text] -> Bool
isProbe = any suspicious . map T.toLower
  where
    suspicious s =
      s `elem` names
        || any (`T.isSuffixOf` s) exts
        || ("." `T.isPrefixOf` s && s /= ".well-known")
        || "wp-" `T.isPrefixOf` s
    names = ["wordpress", "phpmyadmin", "pma", "myadmin", "cgi-bin", "administrator", "vendor", "actuator", "boaform", "hnap1", "autodiscover", "owa", "ecp", "console", "server-status"]
    exts = [".php", ".asp", ".aspx", ".jsp", ".cgi", ".env", ".ini", ".sql", ".bak", ".old", ".swp", ".git", ".zip", ".tar.gz", ".yml", ".yaml", ".config"]
