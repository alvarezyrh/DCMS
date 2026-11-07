-- | Hash password (bcrypt), token acak, dan hash token sesi.
module CMS.Auth
  ( hashPassword
  , checkPassword
  , randomToken
  , hashToken
  , sessionDays
  ) where

import Crypto.Hash (SHA256 (..), hashWith)
import qualified Crypto.KDF.BCrypt as BCrypt
import Crypto.Random (getRandomBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

sessionDays :: Int
sessionDays = 7

hashPassword :: Text -> IO Text
hashPassword pw = do
  h <- BCrypt.hashPassword 11 (TE.encodeUtf8 pw) :: IO ByteString
  pure (TE.decodeUtf8 h)

checkPassword :: Text -> Text -> Bool
checkPassword pw h =
  either (const False) id $
    BCrypt.validatePasswordEither (TE.encodeUtf8 pw) (TE.encodeUtf8 h)

-- | 32 byte acak dalam heksadesimal.
randomToken :: IO Text
randomToken = do
  bytes <- getRandomBytes 32 :: IO ByteString
  pure (TE.decodeUtf8 (B16.encode bytes))

-- | Token sesi tidak disimpan mentah di database.
hashToken :: Text -> Text
hashToken = T.pack . show . hashWith SHA256 . TE.encodeUtf8
