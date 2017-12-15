module HIP
  (
    resizeImage
  ) where

import           Data.ByteString.Char8     (ByteString)
import qualified Data.ByteString.Lazy      as LB (ByteString, toStrict)
import           Data.Int                  (Int64)

import           Control.Monad.IO.Class    (liftIO)

import           Graphics.Image            (Border (Edge), Nearest (Nearest),
                                            dims)

import qualified Control.Monad             as M (foldM)
import           Graphics.Image.Types

import           Graphics.Image.Processing (crop, resize)

readImage :: ByteString -> IO (Either String (Image VS RGB Double))
readImage bs = M.foldM reader (Left "") formats

  where formats = [InputBMP, InputGIF, InputHDR, InputJPG, InputPNG, InputTIF, InputPNM, InputTGA]

        reader :: Either String (Image VS RGB Double) -> InputFormat -> IO (Either String (Image VS RGB Double))
        reader (Left err) format =
          return $ either (Left . ((err++"\n")++)) Right (decode format bs)
        reader img         _     = return img

resizeImage :: LB.ByteString -> Int -> IO (Either String LB.ByteString)
resizeImage bs thumb = do
  decoded <- readImage $ LB.toStrict bs
  case decoded of
    Left e -> pure (Left e)
    Right img -> pure . Right . encode OutputJPG [] $ processImage (dims img) thumb img

processImage :: (Int, Int) -> Int -> Image VS RGB Double -> Image VS RGB Double
processImage (h, w) w' img | h > w = processImage (w, w) w' $ crop (0, 0) (w, w) img
                           | otherwise = resize Nearest Edge (height (h, w), w') img

  where height :: (Int, Int) -> Int
        height (h, w) = w' * h `div` w