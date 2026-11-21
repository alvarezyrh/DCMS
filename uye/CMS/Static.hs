-- | CSS/JS dimuat sekali ke memori saat start: komentar dibuang, versi dari hash isi
-- (cache-busting otomatis), dan versi gzip disiapkan. Perubahan file butuh restart.
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
import System.FilePath ((</>))

data StaticFile = StaticFile
  { sfType :: B.ByteString
  , sfVersion :: B.ByteString
  , sfPlain :: BL.ByteString
  , sfGzip :: BL.ByteString
  }

type StaticFiles = M.Map Text StaticFile

loadStatic :: FilePath -> IO StaticFiles
loadStatic dir = M.fromList <$> mapM load files
  where
    files =
      [ ("style.css", "text/css; charset=utf-8", stripCssComments)
      , ("app.js", "text/javascript; charset=utf-8", stripJsLineComments)
      , ("builder.js", "text/javascript; charset=utf-8", stripJsLineComments)
      ]
    load (name, ct, prep) = do
      body <- prep <$> B.readFile (dir </> T.unpack name)
      let ver = BC.pack (take 10 (show (hashWith SHA256 body)))
          lazy = BL.fromStrict body
      pure (name, StaticFile ct ver lazy (GZ.compress lazy))

-- | URL berversi, mis. "/static/style.css?v=3fa9c0d1e2". Berubah otomatis bila isi berubah.
staticUrl :: StaticFiles -> Text -> Text
staticUrl fs name =
  "/static/" <> name <> maybe "" (("?v=" <>) . TE.decodeUtf8 . sfVersion) (M.lookup name fs)

serveStatic :: StaticFiles -> Request -> Text -> Response
serveStatic fs req name = case M.lookup name fs of
  Nothing -> responseLBS status404 [] "not found"
  Just f
    | lookup "If-None-Match" hs == Just (etag f) -> responseLBS status304 (common f) ""
    | acceptsGzip -> responseLBS status200 ((hContentEncoding, "gzip") : typed f) (sfGzip f)
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
