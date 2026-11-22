-- | Penulis arsip ZIP minimal (DEFLATE), cukup untuk berkas Office Open XML (.xlsx).
module CMS.Zip (zipArchive) where

import qualified Codec.Compression.Zlib.Raw as Raw
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word16, Word32)

-- | Arsip dari daftar (nama berkas, isi). Tanggal berkas memakai 1 Jan 1980 (nilai
-- bawaan ZIP), sehingga isi yang sama selalu menghasilkan arsip yang sama persis.
zipArchive :: [(Text, BL.ByteString)] -> BL.ByteString
zipArchive files = BB.toLazyByteString (mconcat locals <> mconcat centrals <> end)
  where
    entries = map entry files
    offsets = scanl (+) 0 [fromIntegral (BL.length (BB.toLazyByteString l)) | (l, _) <- entries]
    locals = map fst entries
    centrals = zipWith (\(_, cen) off -> cen off) entries offsets
    centralSize = sum [BL.length (BB.toLazyByteString (cen 0)) | (_, cen) <- entries]
    end =
      w32 0x06054b50 <> w16 0 <> w16 0
        <> w16 (fromIntegral (length files)) <> w16 (fromIntegral (length files))
        <> w32 (fromIntegral centralSize) <> w32 (fromIntegral (last offsets)) <> w16 0

-- | Header lokal + data, dan header direktori pusat (butuh offset header lokalnya).
entry :: (Text, BL.ByteString) -> (BB.Builder, Word32 -> BB.Builder)
entry (name, raw) = (local, central)
  where
    nameB = TE.encodeUtf8 (T.dropWhile (== '/') name)
    packed = Raw.compress raw
    crc = crc32 raw
    common =
      w16 20 <> w16 0x0800 <> w16 8 <> w16 0 <> w16 0x21 -- versi, flag UTF-8, DEFLATE, jam, tanggal 1980-01-01
        <> w32 crc <> w32 (fromIntegral (BL.length packed)) <> w32 (fromIntegral (BL.length raw))
        <> w16 (fromIntegral (B.length nameB)) <> w16 0
    local = w32 0x04034b50 <> common <> BB.byteString nameB <> BB.lazyByteString packed
    central off =
      w32 0x02014b50 <> w16 20 <> common <> w16 0 <> w16 0 <> w16 0 <> w32 0 <> w32 off <> BB.byteString nameB

w16 :: Word16 -> BB.Builder
w16 = BB.word16LE

w32 :: Word32 -> BB.Builder
w32 = BB.word32LE

crc32 :: BL.ByteString -> Word32
crc32 = xor 0xffffffff . BL.foldl' step 0xffffffff
  where
    step c b = (crcTable ! fromIntegral ((c `xor` fromIntegral b) .&. 0xff)) `xor` (c `shiftR` 8)

crcTable :: UArray Int Word32
crcTable = listArray (0, 255) [foldl' (\c _ -> if c .&. 1 == 1 then 0xedb88320 `xor` (c `shiftR` 1) else c `shiftR` 1) n [1 .. 8 :: Int] | n <- [0 .. 255]]
