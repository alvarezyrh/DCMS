-- | Aset di folder static (CSS, JS, font woff2) dimuat sekali ke memori saat start:
-- komentar dibuang, versi dari hash isi (cache-busting otomatis), dan versi gzip
-- disiapkan untuk teks. url(...) di CSS yang menunjuk aset lain ditulis ulang ke alamat
-- berversinya, sehingga font juga bisa di-cache permanen. Perubahan file butuh restart.
module CMS.Static (StaticFile, StaticFiles, loadStatic, serveStatic, staticUrl) where

import qualified Codec.Compression.GZip as GZ
import Control.Monad (join)
import Crypto.Hash (SHA256 (..), hashWith)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
import Network.Wai
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

data StaticFile = StaticFile
  { sfType :: B.ByteString
  , sfVersion :: B.ByteString
  , sfPlain :: BL.ByteString
  , sfGzip :: Maybe BL.ByteString -- Nothing untuk berkas yang sudah terkompresi (woff2)
  }

type StaticFiles = M.Map Text StaticFile

loadStatic :: FilePath -> IO StaticFiles
loadStatic dir = do
  names <- listDirectory dir
  files <- sequence [(,,) n k <$> B.readFile (dir </> n) | n <- names, Just k <- [kindOf n]]
  -- CSS terakhir: url(...) di dalamnya butuh versi aset lain.
  let assets = M.fromList [(T.pack n, asset n k body) | (n, k, body) <- files, takeExtension n /= ".css"]
      sheets = M.fromList [(T.pack n, asset n k (versionUrls assets (stripCssComments body))) | (n, k, body) <- files, takeExtension n == ".css"]
  pure (assets <> sheets)
  where
    asset n (ct, gz) body =
      let prepared = if takeExtension n == ".js" then stripJsLineComments body else body
          lazy = BL.fromStrict prepared
       in StaticFile ct (BC.pack (take 10 (show (hashWith SHA256 prepared)))) lazy (if gz then Just (GZ.compress lazy) else Nothing)

-- | Jenis aset yang disajikan: (Content-Type, perlu gzip?).
kindOf :: FilePath -> Maybe (B.ByteString, Bool)
kindOf n = case takeExtension n of
  ".css" -> Just ("text/css; charset=utf-8", True)
  ".js" -> Just ("text/javascript; charset=utf-8", True)
  ".woff2" -> Just ("font/woff2", False)
  _ -> Nothing

-- | url(nama) yang menunjuk aset lain -> url(/static/nama?v=versi).
versionUrls :: StaticFiles -> B.ByteString -> B.ByteString
versionUrls fs css =
  TE.encodeUtf8 $
    M.foldrWithKey (\name _ t -> T.replace ("url(" <> name <> ")") ("url(" <> staticUrl fs name <> ")") t) (TE.decodeUtf8 css) fs

-- | URL berversi, mis. "/static/style.css?v=3fa9c0d1e2". Berubah otomatis bila isi berubah.
staticUrl :: StaticFiles -> Text -> Text
staticUrl fs name =
  "/static/" <> name <> maybe "" (("?v=" <>) . TE.decodeUtf8 . sfVersion) (M.lookup name fs)

serveStatic :: StaticFiles -> Request -> Text -> Response
serveStatic fs req name = case M.lookup name fs of
  Nothing -> responseLBS status404 [] "not found"
  Just f
    | lookup "If-None-Match" hs == Just (etag f) -> responseLBS status304 (common f) ""
    | Just gz <- sfGzip f, acceptsGzip -> responseLBS status200 ((hContentEncoding, "gzip") : typed f) gz
    | otherwise -> responseLBS status200 (typed f) (sfPlain f)
  where
    hs = requestHeaders req
    acceptsGzip = maybe False (B.isInfixOf "gzip") (lookup hAcceptEncoding hs)
    versioned f = join (lookup "v" (queryString req)) == Just (sfVersion f)
    etag f = "\"" <> sfVersion f <> "\""
    common f =
      [ (hETag, etag f)
      , (hVary, "Accept-Encoding")
      , (hCacheControl, if versioned f then "public, max-age=31536000, immutable" else "no-cache")
      ]
    typed f = (hContentType, sfType f) : common f

-- | Buang komentar /* ... */ di luar string.
stripCssComments :: B.ByteString -> B.ByteString
stripCssComments = BC.pack . go . BC.unpack
  where
    go ('/' : '*' : rest) = go (dropComment rest)
    go (q : rest) | q == '"' || q == '\'' = let (s, r) = str q rest in q : s ++ go r
    go (ch : rest) = ch : go rest
    go [] = []
    dropComment ('*' : '/' : rest) = rest
    dropComment (_ : rest) = dropComment rest
    dropComment [] = []
    str q ('\\' : ch : rest) = let (s, r) = str q rest in ('\\' : ch : s, r)
    str q (ch : rest)
      | ch == q = ([ch], rest)
      | otherwise = let (s, r) = str q rest in (ch : s, r)
    str _ [] = ([], [])

-- | Buang baris yang hanya berisi komentar //. Aman selama tidak ada template literal
-- multi-baris (`...`) yang barisnya diawali //.
stripJsLineComments :: B.ByteString -> B.ByteString
stripJsLineComments = BC.unlines . filter (not . ("//" `B.isPrefixOf`) . BC.dropWhile (== ' ')) . BC.lines
