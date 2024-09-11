{-# LANGUAGE OverloadedLists #-}

module ImageRider (runProdosHeaderParser, runProdosParser, someFunc) where

import qualified Data.ByteString as BS
import Data.Void (Void)
import ImageRider.DiskImage.Apple.Prodos as Prodos (MagicIdentifier (..), ProdosHeader (..), ProdosImage (..), parseHeader, parseImage)
import Text.Megaparsec (ParseErrorBundle, eof, parseTest, runParser)

readImage :: FilePath -> IO BS.ByteString
readImage filename = do
  BS.readFile filename

runProdosParser :: BS.ByteString -> Either (ParseErrorBundle BS.ByteString Void) Prodos.ProdosImage
runProdosParser image_data = do
  runParser Prodos.parseImage "ProDOS image" image_data

runProdosHeaderParser :: BS.ByteString -> Either (ParseErrorBundle BS.ByteString Void) Prodos.ProdosHeader
runProdosHeaderParser image_data = do
  runParser Prodos.parseHeader "ProDOS header" (BS.take 5 image_data)

someFunc :: FilePath -> IO ()
someFunc filename = do
  image_data <- readImage filename
  case runProdosParser image_data of
    Left errorMessage -> print $ errorMessage
    Right res -> print $ res
