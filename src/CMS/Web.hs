-- | Bantuan HTTP: parameter, respons, cookie, flash.
module CMS.Web where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (secondsToDiffTime)
import Lucid (Html, renderBS)
import Network.HTTP.Types
import Network.Wai
import Web.Cookie

import CMS.Types
import CMS.Util (readInt)

-- Parameter -------------------------------------------------------------------

param :: Ctx -> Text -> Maybe Text
param c k = lookup k (cForm c) <|> lookup k (cQuery c)

-- | Nilai parameter yang sudah di-trim (kosong bila tidak ada).
paramT :: Ctx -> Text -> Text
paramT c k = T.strip (fromMaybe "" (param c k))

-- | Nilai mentah, tanpa trim (untuk password dan isi artikel).
paramRaw :: Ctx -> Text -> Text
paramRaw c k = fromMaybe "" (param c k)

paramInt :: Ctx -> Text -> Maybe Int
paramInt c k = param c k >>= readInt . T.strip

paramBool :: Ctx -> Text -> Bool
paramBool c k = maybe False (`elem` ["1", "on", "true"]) (param c k)

-- | Halaman saat ini dari ?page=
pageParam :: Ctx -> Int
pageParam c = maybe 1 (max 1) (paramInt c "page")

-- Respons -----------------------------------------------------------------------

htmlResp :: Status -> Html () -> Response
htmlResp st h = responseLBS st [(hContentType, "text/html; charset=utf-8")] (renderBS h)

ok :: Html () -> IO Response
ok = pure . htmlResp status200

textResp :: Status -> Text -> Response
textResp st t = responseLBS st [(hContentType, "text/plain; charset=utf-8")] (BL.fromStrict (TE.encodeUtf8 t))

redirectTo :: Text -> Response
redirectTo url = responseLBS status303 [(hLocation, TE.encodeUtf8 url)] ""

-- | Redirect dengan pesan flash.
redirectFlash :: Text -> Text -> Text -> IO Response
redirectFlash url kind msg = pure (withFlash kind msg (redirectTo url))

-- Cookie & flash ---------------------------------------------------------------

addHeader :: Header -> Response -> Response
addHeader h = mapResponseHeaders (h :)

cookieHeader :: ByteString -> ByteString -> Maybe Integer -> Header
cookieHeader name val maxAge =
  ( "Set-Cookie"
  , BL.toStrict . BB.toLazyByteString . renderSetCookie $
      defaultSetCookie
        { setCookieName = name
        , setCookieValue = val
        , setCookiePath = Just "/"
        , setCookieHttpOnly = True
        , setCookieSameSite = Just sameSiteLax
        , setCookieMaxAge = secondsToDiffTime <$> maxAge
        }
  )

clearCookie :: ByteString -> Header
clearCookie name = cookieHeader name "" (Just 0)

withFlash :: Text -> Text -> Response -> Response
withFlash kind msg =
  addHeader (cookieHeader "flash" (urlEncode True (TE.encodeUtf8 (kind <> "|" <> msg))) (Just 60))

decodeFlash :: ByteString -> Maybe Flash
decodeFlash raw =
  case T.breakOn "|" (TE.decodeUtf8Lenient (urlDecode True raw)) of
    (k, rest) | not (T.null rest) -> Just (Flash k (T.drop 1 rest))
    _ -> Nothing

setsCookie :: ByteString -> Response -> Bool
setsCookie name resp =
  any
    (\(h, v) -> h == "Set-Cookie" && (name <> "=") `B.isPrefixOf` v)
    (responseHeaders resp)

-- | Bangun URL dengan query string, membuang nilai kosong.
urlWith :: Text -> [(Text, Text)] -> Text
urlWith base kvs =
  let kvs' = filter (not . T.null . snd) kvs
      enc = TE.decodeUtf8 . urlEncode True . TE.encodeUtf8
   in if null kvs' then base else base <> "?" <> T.intercalate "&" [k <> "=" <> enc v | (k, v) <- kvs']

-- | Hanya izinkan redirect lokal (mencegah open redirect).
safeNext :: Text -> Text
safeNext t
  | "/" `T.isPrefixOf` t && not ("//" `T.isPrefixOf` t) && not ("/\\" `T.isPrefixOf` t) = t
  | otherwise = "/admin"

-- | Alamat situs dari request, mis. "https://contoh.id" (untuk URL absolut: canonical,
-- sitemap, Open Graph). Header Host dibersihkan agar tidak bisa menyisipkan apa pun.
siteOrigin :: Ctx -> Text
siteOrigin c = case envSiteUrl (cEnv c) of
  Just u -> u
  Nothing -> originFromRequest c

originFromRequest :: Ctx -> Text
originFromRequest c =
  let req = cReq c
      host = T.filter ok (maybe "localhost" TE.decodeUtf8Lenient (requestHeaderHost req))
      proto = lookup "X-Forwarded-Proto" (requestHeaders req)
      scheme = if isSecure req || proto == Just "https" then "https" else "http"
   in scheme <> "://" <> (if T.null host then "localhost" else T.take 255 host)
  where
    ok ch = ch `elem` ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-:[]" :: String)

-- | IP pengunjung. Di belakang reverse proxy (CMS_TRUST_PROXY=1) diambil dari entri
-- terakhir X-Forwarded-For, yaitu yang ditambahkan proxy kita sendiri.
clientIp :: Ctx -> Text
clientIp c
  | envTrustProxy (cEnv c)
  , Just xff <- lookup "X-Forwarded-For" (requestHeaders (cReq c))
  , ip : _ <- reverse (filter (not . T.null) (map T.strip (T.splitOn "," (TE.decodeUtf8Lenient xff)))) =
      T.take 64 ip
  | otherwise =
      let a = T.pack (show (remoteHost (cReq c)))
       in if "[" `T.isPrefixOf` a then T.takeWhile (/= ']') (T.drop 1 a) else T.takeWhile (/= ':') a
