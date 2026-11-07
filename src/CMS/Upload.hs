-- | Aset gambar: unggah, hapus, dan sajikan file dari folder aset (src/asset/img).
module CMS.Upload
  ( saveImage
  , removeAsset
  , serveAsset
  , assetPrefix
  , maxImageBytes
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (void, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai (Response, responseFile, responseLBS)
import Network.Wai.Parse (FileInfo (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))

import CMS.Auth (randomToken)
import CMS.Types

-- | URL publik folder aset.
assetPrefix :: Text
assetPrefix = "/asset/img/"

maxImageBytes :: Int
maxImageBytes = 2 * 1024 * 1024

-- | Simpan gambar unggahan dengan nama acak, mis. "avatar-3f9c…a1.jpg".
-- Jenis file dicek dari isinya (bukan dari nama/Content-Type). SVG ditolak.
saveImage :: Env -> Text -> FileInfo BL.ByteString -> IO (Either Text Text)
saveImage env prefix fi
  | BL.null bytes = pure (Left "Pilih berkas gambar terlebih dahulu.")
  | BL.length bytes > fromIntegral maxImageBytes = pure (Left "Ukuran gambar maksimal 2 MB.")
  | otherwise = case detectImage bytes of
      Nothing -> pure (Left "Format tidak didukung. Gunakan JPG, PNG, WebP, atau GIF.")
      Just ext -> do
        tok <- randomToken
        let name = prefix <> "-" <> T.take 20 tok <> "." <> ext
        createDirectoryIfMissing True (envAssetDir env)
        BL.writeFile (envAssetDir env </> T.unpack name) bytes
        pure (Right (assetPrefix <> name))
  where
    bytes = fileContent fi

detectImage :: BL.ByteString -> Maybe Text
detectImage b
  | BL.take 8 b == BL.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A] = Just "png"
  | BL.take 3 b == BL.pack [0xFF, 0xD8, 0xFF] = Just "jpg"
  | BL.take 4 b == BL.pack [0x47, 0x49, 0x46, 0x38] = Just "gif"
  | BL.take 4 b == BL.pack [0x52, 0x49, 0x46, 0x46] && BL.take 4 (BL.drop 8 b) == BL.pack [0x57, 0x45, 0x42, 0x50] = Just "webp"
  | otherwise = Nothing

-- | Hapus file unggahan lama (hanya file hasil unggahan di folder aset).
removeAsset :: Env -> Text -> IO ()
removeAsset env url = case T.stripPrefix assetPrefix url of
  Just name
    | safeName name && T.any (== '-') name && name /= "placeholder.svg" -> do
        let path = envAssetDir env </> T.unpack name
        exists <- doesFileExist path
        when exists $ void (try (removeFile path) :: IO (Either SomeException ()))
  _ -> pure ()

-- | Nama file aman: huruf kecil/angka/-/_/., tidak diawali titik, ekstensi gambar.
safeName :: Text -> Bool
safeName n =
  not (T.null n)
    && T.head n /= '.'
    && not (".." `T.isInfixOf` n)
    && T.all (\ch -> isAsciiLower ch || isDigit ch || ch `elem` ("-_." :: String)) n
    && maybe False (const True) (lookup (extOf n) mimeTypes)

-- | GET /asset/img/<nama>
serveAsset :: Env -> Text -> IO Response
serveAsset env name = case (safeName name, ctype) of
  (True, Just ct) -> do
    let path = envAssetDir env </> T.unpack name
    exists <- doesFileExist path
    pure $
      if exists
        then
          responseFile
            status200
            [ (hContentType, ct)
            , (hCacheControl, "public, max-age=86400")
            , ("X-Content-Type-Options", "nosniff")
            , ("Content-Security-Policy", "default-src 'none'; style-src 'unsafe-inline'; sandbox")
            ]
            path
            Nothing
        else notFound
  _ -> pure notFound
  where
    ctype = lookup (extOf name) mimeTypes
    notFound = responseLBS status404 [(hContentType, "text/plain")] "not found"

extOf :: Text -> Text
extOf = T.toLower . snd . T.breakOnEnd "."

mimeTypes :: [(Text, B.ByteString)]
mimeTypes =
  [ ("png", "image/png")
  , ("jpg", "image/jpeg")
  , ("jpeg", "image/jpeg")
  , ("gif", "image/gif")
  , ("webp", "image/webp")
  , ("svg", "image/svg+xml")
  ]

