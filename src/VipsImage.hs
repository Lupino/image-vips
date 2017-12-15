{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module VipsImage
  (
    VipsImage
  , Error
  , vipsInit
  , vipsShutdown
  , openImage
  , vipsThumbnailImage
  , saveImage
  , saveImage'
  , closeImage
  ) where

import           Control.Exception            (Exception, throwIO)
import           Control.Monad                (when)
import qualified Data.ByteString              as BS (ByteString, empty, pack)
import           Data.IORef                   (IORef, newIORef, readIORef,
                                               writeIORef)
import           Data.Monoid                  ((<>))
import           Foreign.C.Types
import           Foreign.ForeignPtr           (newForeignPtr_)
import           Foreign.Ptr                  (Ptr)
import qualified Language.C.Inline            as C

import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.Storable             (Storable)

C.context (C.baseCtx <> C.bsCtx <> C.fptrCtx <> C.funCtx)

C.include "<stdio.h>"
C.include "<vips/vips.h>"

newtype VipsImage = VipsImage (Ptr ())

newtype Error = Error String
  deriving (Show, Eq, Ord)

instance Exception Error

checkError :: String -> CInt -> IO ()
checkError e n = when (n /= 0) $ throwIO (Error e)

vipsInit :: BS.ByteString -> IO ()
vipsInit bs =
  [C.exp| int {vips_init($bs-ptr:bs)} |] >>=
    checkError "vips_init failed"

vipsShutdown :: IO ()
vipsShutdown = [C.exp| void {vips_shutdown()}|]

openImage :: BS.ByteString -> IO VipsImage
openImage bs = VipsImage <$> [C.exp| void * {vips_image_new_from_file($bs-ptr:bs, NULL)}|]

closeImage :: VipsImage -> IO ()
closeImage (VipsImage img) = do
  ptr <- newForeignPtr_ img
  [C.exp| void { g_object_unref($fptr-ptr:(void *ptr)) }|]

vipsThumbnailImage :: VipsImage -> CInt -> IO VipsImage
vipsThumbnailImage (VipsImage in_) w = VipsImage <$> do
  ptr <- newForeignPtr_ in_
  [C.block| void * {
    VipsImage *out;
    int r = vips_thumbnail_image($fptr-ptr:(void *ptr), &out, $(int w), NULL);
    return $fun:(void * (* funIO) (int r, void *out))(r, out);
  }|]

  where funIO :: CInt -> Ptr () -> IO (Ptr ())
        funIO r ptr = do
          checkError "vips_thumbnail_image failed" r
          pure ptr


saveImage :: VipsImage -> BS.ByteString -> IO ()
saveImage (VipsImage in_) fn = do
  ptr <- newForeignPtr_ in_
  [C.exp| int {vips_image_write_to_file($fptr-ptr:(void *ptr), $bs-ptr:fn, NULL)} |] >>=
    checkError "vips_image_write_to_file failed"


saveImage' :: VipsImage -> IO BS.ByteString
saveImage' (VipsImage img) = do
  ptr <- newForeignPtr_ img
  h <- newIORef BS.empty
  let wIO = writeIO h
  [C.block| int {
    void *buf;
    size_t len;
    int r = vips_image_write_to_buffer($fptr-ptr:(void *ptr), ".jpg", &buf, &len, NULL);
    if (r) {
      return r;
    }
    return $fun:(int (*wIO)(char *buf, size_t len))(buf, len);
  }|] >>=
    checkError "vips_image_write_to_buffer failed"
  readIORef h

  where writeIO :: IORef BS.ByteString -> Ptr CChar -> CSize -> IO CInt
        writeIO h ptr s = do
          v <- vectorFromC (fromIntegral s) ptr
          writeIORef h . BS.pack . map fromIntegral $ V.toList v
          pure 0

vectorFromC :: Storable a => Int -> Ptr a -> IO (V.Vector a)
vectorFromC len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.freeze $ VM.unsafeFromForeignPtr0 ptr' len
