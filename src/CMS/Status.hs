-- | Status server untuk dashboard Administrator.
module CMS.Status
  ( ServerStatus (..)
  , getServerStatus
  , statusPairs
  ) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import GHC.Conc (getNumCapabilities, getNumProcessors)
import System.Directory (doesFileExist, getFileSize, listDirectory)
import System.FilePath ((</>))
import System.Info (arch, fullCompilerVersion, os)
import Data.Version (showVersion)

import CMS.DB (countActiveSessions, withDb)
import CMS.Types

data ServerStatus = ServerStatus
  { ssDbOk :: Bool
  , ssUptime :: Text
  , ssStarted :: Text
  , ssMemory :: Text
  , ssLoad :: Text
  , ssCpu :: Text
  , ssDbSize :: Text
  , ssAssets :: Text
  , ssSessions :: Int
  , ssRuntime :: Text
  , ssTime :: Text
  }

getServerStatus :: Env -> IO ServerStatus
getServerStatus env = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  sess <- try (withDb env countActiveSessions) :: IO (Either SomeException Int)
  rss <- procField "/proc/self/status" "VmRSS:"
  load <- firstLine "/proc/loadavg"
  caps <- getNumCapabilities
  procs <- getNumProcessors
  dbBytes <- sum <$> mapM fileSize [envDbPath env, envDbPath env <> "-wal"]
  (nFiles, assetBytes) <- folderStats (envAssetDir env)
  let local t = T.pack (formatTime defaultTimeLocale "%d %b %Y, %H:%M:%S" (utcToLocalTime tz t))
  pure
    ServerStatus
      { ssDbOk = either (const False) (const True) sess
      , ssUptime = fmtDuration (floor (diffUTCTime now (envStarted env)))
      , ssStarted = local (envStarted env)
      , ssMemory = maybe "—" (\kb -> fmtBytes (kb * 1024)) (rss >>= readKb)
      , ssLoad = maybe "—" (T.unwords . take 3 . T.words) load
      , ssCpu = tshow' procs <> " inti · " <> tshow' caps <> " thread RTS"
      , ssDbSize = fmtBytes dbBytes
      , ssAssets = tshow' nFiles <> " file · " <> fmtBytes assetBytes
      , ssSessions = either (const 0) id sess
      , ssRuntime = "GHC " <> T.pack (showVersion fullCompilerVersion) <> " · " <> T.pack os <> "/" <> T.pack arch
      , ssTime = local now
      }

-- | Untuk JSON (pembaruan otomatis di dashboard).
statusPairs :: ServerStatus -> [(Text, Text)]
statusPairs s =
  [ ("db", if ssDbOk s then "Terhubung" else "Gangguan")
  , ("uptime", ssUptime s)
  , ("started", ssStarted s)
  , ("memory", ssMemory s)
  , ("load", ssLoad s)
  , ("cpu", ssCpu s)
  , ("dbsize", ssDbSize s)
  , ("assets", ssAssets s)
  , ("sessions", tshow' (ssSessions s))
  , ("runtime", ssRuntime s)
  , ("time", ssTime s)
  ]

-- Bantu ------------------------------------------------------------------------------------

tshow' :: Show a => a -> Text
tshow' = T.pack . show

safeRead :: FilePath -> IO (Maybe Text)
safeRead p = either (const Nothing) Just <$> (try (TIO.readFile p) :: IO (Either SomeException Text))

firstLine :: FilePath -> IO (Maybe Text)
firstLine p = fmap (T.strip . T.takeWhile (/= '\n')) <$> safeRead p

-- | Nilai baris "Kunci:   123 kB" dari file /proc (Linux).
procField :: FilePath -> Text -> IO (Maybe Text)
procField p key = do
  mt <- safeRead p
  pure $ do
    t <- mt
    line <- lookupLine (T.lines t)
    pure (T.strip (T.drop (T.length key) line))
  where
    lookupLine ls = case filter (key `T.isPrefixOf`) ls of
      (l : _) -> Just l
      [] -> Nothing

readKb :: Text -> Maybe Integer
readKb t = case reads (T.unpack (T.takeWhile (/= ' ') t)) of
  [(n, "")] -> Just n
  _ -> Nothing

fileSize :: FilePath -> IO Integer
fileSize p = do
  ex <- doesFileExist p
  if ex then either (const 0) id <$> (try (getFileSize p) :: IO (Either SomeException Integer)) else pure 0

folderStats :: FilePath -> IO (Int, Integer)
folderStats dir = do
  r <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
  case r of
    Left _ -> pure (0, 0)
    Right names -> do
      sizes <- mapM (fileSize . (dir </>)) names
      pure (length names, sum sizes)

fmtBytes :: Integer -> Text
fmtBytes b
  | b >= gb = one (fromIntegral b / fromIntegral gb) <> " GB"
  | b >= mb = one (fromIntegral b / fromIntegral mb) <> " MB"
  | b >= kb = one (fromIntegral b / fromIntegral kb) <> " KB"
  | otherwise = tshow' b <> " B"
  where
    kb = 1024
    mb = kb * 1024
    gb = mb * 1024
    one :: Double -> Text
    one x = T.pack (show (fromIntegral (round (x * 10) :: Integer) / 10 :: Double))

-- | 93784 detik -> "1 hari 2 jam 3 menit"
fmtDuration :: Integer -> Text
fmtDuration s
  | d > 0 = T.unwords [tshow' d <> " hari", tshow' h <> " jam", tshow' m <> " menit"]
  | h > 0 = T.unwords [tshow' h <> " jam", tshow' m <> " menit"]
  | m > 0 = T.unwords [tshow' m <> " menit", tshow' sec <> " detik"]
  | otherwise = tshow' sec <> " detik"
  where
    (d, r1) = s `divMod` 86400
    (h, r2) = r1 `divMod` 3600
    (m, sec) = r2 `divMod` 60
