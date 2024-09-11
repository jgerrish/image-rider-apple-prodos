{-# LANGUAGE OverloadedLists #-}
-- :set -XOverloadedLists

module Main (main) where

import qualified Data.ByteString as BS (pack)
import qualified Data.Either as DE (isLeft)
import ImageRider (runProdosHeaderParser)
import qualified ImageRider.DiskImage.Apple.Prodos as Prodos (MagicIdentifier (..), ProdosHeader (..), parseHeader)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

main :: IO ()
main = hspec $ do
  -- These tests just use the hspec package for testing
  describe "Test parsing magic identifier" $ do
    it "valid header is accepted" $ do
      let expected_magic = Right (Prodos.ProdosHeader (Prodos.MagicIdentifier [0x01, 0x38, 0xB0, 0x03, 0x4C]))
          actual_magic = runProdosHeaderParser [0x01, 0x38, 0xB0, 0x03, 0x4C]
      actual_magic `shouldBe` expected_magic

  describe "Test parsing invalid magic identifier" $ do
    it "invalid header is rejected" $ do
      -- isLeft matches on a Left (error) result without being more
      -- specific
      (DE.isLeft (runProdosHeaderParser [0x01, 0x38, 0xB0, 0x02, 0x4C]))
        `shouldBe` True

  -- These tests use the hspec-megaparsec package
  describe "parseHeader" $ do
    it "should parse a valid header" $ do
      parse Prodos.parseHeader "" image `shouldParse` expected
    it "should parse a valid header and satisfy the expected result" $ do
      parse Prodos.parseHeader "" image `parseSatisfies` (== expected)
    it "should fail on an invalid header" $ do
      parse Prodos.parseHeader "" `shouldFailOn` bad_image
  where
    expected = (Prodos.ProdosHeader (Prodos.MagicIdentifier (BS.pack [0x01, 0x38, 0xB0, 0x03, 0x4C])))
    image = (BS.pack [0x01, 0x38, 0xB0, 0x03, 0x4C])
    bad_image = (BS.pack [0x01, 0x38, 0xB0, 0x02, 0x4C])
