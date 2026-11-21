-- | Pembatas percobaan login di memori: setelah terlalu banyak password salah dalam
-- 15 menit, kunci sementara. Data hilang saat server dimulai ulang (sengaja: cukup
-- untuk menghambat tebak-tebakan password tanpa tabel tambahan).
module CMS.RateLimit
  ( Limiter
  , newLimiter
  , blockedFor
  , recordFailure
  , clearKeys
  , windowSeconds
  ) where

import Control.Concurrent.MVar
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)

newtype Limiter = Limiter (MVar (M.Map Text [UTCTime]))

newLimiter :: IO Limiter
newLimiter = Limiter <$> newMVar M.empty

windowSeconds :: NominalDiffTime
windowSeconds = 15 * 60

-- | Untuk tiap (kunci, batas): bila percobaan gagal dalam jendela waktu sudah
-- mencapai batas, kembalikan sisa detik sampai boleh mencoba lagi.
blockedFor :: Limiter -> [(Text, Int)] -> IO (Maybe Int)
blockedFor (Limiter mv) rules = do
  now <- getCurrentTime
  m <- readMVar mv
  let waits =
        [ ceiling (windowSeconds - diffUTCTime now oldest)
        | (k, limit) <- rules
        , let ts = recent now (M.findWithDefault [] k m)
        , length ts >= limit
        , let oldest = minimum ts
        ]
  pure (if null waits then Nothing else Just (max 1 (maximum waits)))

recordFailure :: Limiter -> [Text] -> IO ()
recordFailure (Limiter mv) keys = do
  now <- getCurrentTime
  modifyMVar_ mv $ \m -> do
    let m' = foldr (\k -> M.insertWith (\new old -> new ++ take 50 (recent now old)) k [now]) m keys
    -- Bersihkan kunci kedaluwarsa agar memori tidak terus bertambah.
    pure (if M.size m' > 5000 then M.filter (not . null) (M.map (recent now) m') else m')

clearKeys :: Limiter -> [Text] -> IO ()
clearKeys (Limiter mv) keys = modifyMVar_ mv (\m -> pure (foldr M.delete m keys))

recent :: UTCTime -> [UTCTime] -> [UTCTime]
recent now = filter (> addUTCTime (negate windowSeconds) now)
