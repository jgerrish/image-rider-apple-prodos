{-# LANGUAGE OverloadedLists #-}
-- :set -XOverloadedStrings

module ImageRider.DiskImage.Apple.Prodos (parseHeader, parseImage, MagicIdentifier (..), Parser, ProdosHeader (..), ProdosImage (..)) where

import Control.Monad (when)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS (ByteString, unpack)
import qualified Data.List as DL (intercalate)
import Data.Void
import Text.Megaparsec (Parsec, anySingle, takeP, try, (<?>), (<|>))
import qualified Text.Printf as PF (printf)

-- TODO: Describe this
-- For example, it appears to store any type with the MagicIdentifier
-- constructor.  It's only getMagicIdentifier that coerces it to
-- ByteString
newtype MagicIdentifier = MagicIdentifier {getMagicIdentifier :: BS.ByteString} deriving (Eq)

instance Show MagicIdentifier where
  show x =
    "["
      ++ (DL.intercalate ", " ((PF.printf "0x%02X") <$> (BS.unpack (getMagicIdentifier x))))
      ++ "]"

data VolumeDirectoryBlock = VolumeDirectoryBlock
  { label :: BS.ByteString,
    storageType :: Int,
    volumeName :: BS.ByteString
  }
  deriving (Eq)

instance Show VolumeDirectoryBlock where
  show x =
    "volume directory block header: ["
      ++ (DL.intercalate ", " ((PF.printf "0x%02X") <$> (BS.unpack (label x))))
      ++ "]"
      ++ ", storageType: "
      ++ show (storageType x)
      ++ ", volume name: "
      ++ show (volumeName x)

data ProdosHeader = ProdosHeader
  { magic :: MagicIdentifier
  }
  deriving (Eq, Show)

data ProdosImage = ProdosImage
  { header :: ProdosHeader,
    volume_directory_block :: VolumeDirectoryBlock
  }
  deriving (Eq, Show)

-- Void is the type of the error component
-- BS.ByteString is the type of the input stream
type Parser = Parsec Void BS.ByteString

-- TODO: Fix the below,  it's still not idiomatic
-- The Intro to Parsing with Parsec in Haskell tutorial has some
-- helpful information on  applicative style parsing code
-- https://github.com/JakeWheat/intro_to_parsing.git
parseMagic :: Parser MagicIdentifier
parseMagic = do
  magic_actual <- (takeP Nothing 5)
  when (magic_actual /= magic_expected) $ fail $ "unexpected start of text\n" <> "expecting end of text"

  return (MagicIdentifier magic_actual)

parseHeader :: Parser ProdosHeader
parseHeader = do
  magic_id <- parseMagic <?> "magic identifier"
  return (ProdosHeader magic_id)

parseVolumeDirectoryBlockHeader :: Parser BS.ByteString
parseVolumeDirectoryBlockHeader = do
  -- The Intro to Parsing with Parsec in Haskell tutorial has some
  -- helpful information on  applicative style parsing code
  -- https://github.com/JakeWheat/intro_to_parsing.git
  --
  -- Here we see the <* operator.  pa <* pb runs pa, then runs pb,
  -- and returns the result of pa.
  -- We use it here because satisfy doesn't return a ByteString
  res <- (takeP Nothing 0x04)
  when ([0x00, 0x00, 0x03, 0x00] /= res) $ fail $ "expected volume directory block header " <> show "[0x00, 0x00, 0x03, 0x00]" <> " but got " <> show res
  return res

parseVolumeDirectoryBlock :: Parser VolumeDirectoryBlock
parseVolumeDirectoryBlock = do
  -- Here we see the *> operator.  pa *> pb runs pa, then runs pb,
  -- and returns the result of pb.  It fails if either of them fails.
  -- We use it here because we don't care about the result of the
  -- takeP "seek".  It's an equivalent to a "drop".
  vbh <-
    try ((takeP Nothing 0x3FB) *> parseVolumeDirectoryBlockHeader)
      <|> ((takeP Nothing 0xAFB) *> parseVolumeDirectoryBlockHeader)

  -- <$> is an in-fix version of the fmap function on Applicative
  -- It's used here to map from a single byte to an integer
  storageTypeAndVolumeNameLength <- fromIntegral <$> anySingle

  let st = (storageTypeAndVolumeNameLength .&. 0xF0) `shiftR` 4
      vnl = storageTypeAndVolumeNameLength .&. 0x0F

  vn <- (takeP Nothing vnl)
  return (VolumeDirectoryBlock vbh st vn)

parseImage :: Parser ProdosImage
parseImage = do
  h <- parseHeader
  vdb <- parseVolumeDirectoryBlock
  return (ProdosImage h vdb)

magic_expected :: BS.ByteString
-- This sugaring is enabled with OverloadedList
magic_expected = [0x01, 0x38, 0xB0, 0x03, 0x4C]
