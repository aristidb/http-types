{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Types.URISpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import qualified Data.ByteString as B

import           Network.HTTP.Types.URI

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "extractPath" $ do

    context "when used with a relative URL" $ do
      it "returns URL unmodified" $ do
        property $ \p -> (not . B.null) p ==>
          extractPath p `shouldBe` p

      context "when path is empty" $ do
        it "returns /" $ do
          extractPath "" `shouldBe` "/"

    context "when used with an absolute URL" $ do
      context "when used with a HTTP URL" $ do
        it "it extracts path" $ do
          extractPath "http://example.com/foo" `shouldBe` "/foo"

        context "when path is empty" $ do
          it "returns /" $ do
            extractPath "http://example.com" `shouldBe` "/"

      context "when used with a HTTPS URL" $ do
        it "it extracts path" $ do
          extractPath "https://example.com/foo" `shouldBe` "/foo"

        context "when path is empty" $ do
          it "returns /" $ do
            extractPath "https://example.com" `shouldBe` "/"
