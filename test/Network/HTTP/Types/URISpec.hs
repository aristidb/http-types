{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Types.URISpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Debug.Trace
import           Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL

import           Network.HTTP.Types.URI

main :: IO ()
main = hspec spec

propEncodeDecodePath :: ([Text], Query) -> Bool
propEncodeDecodePath (p', q') =
    let x = BL.toStrict . B.toLazyByteString $ encodePath a b
        y = decodePath x
        z = y == (a, b)
     in if z then z else traceShow (a, b, x, y) z
  where
    a = if p' == [""] then [] else p'
    b = filter (\(x, _) -> not (B.null x)) q'

propEncodeDecodeQuery :: Query -> Bool
propEncodeDecodeQuery q' =
    q == parseQuery (renderQuery True q)
  where
    q = filter (\(x, _) -> not (B.null x)) q'

propQueryQuestionMark :: (Bool, Query) -> Bool
propQueryQuestionMark (useQuestionMark, query) = actual == expected
    where
      actual = case B8.uncons $ renderQuery useQuestionMark query of
                 Nothing       -> False
                 Just ('?', _) -> True
                 _             -> False
      expected = case (useQuestionMark, null query) of
                   (False, _)    -> False
                   (True, True)  -> False
                   (True, False) -> True

spec :: Spec
spec = do
  describe "encode/decode path" $ do
    it "is identity to encode and then decode" $
      property propEncodeDecodePath
    it "does not escape period and dash" $
      BL.toStrict (B.toLazyByteString (encodePath ["foo-bar.baz"] [])) `shouldBe` "/foo-bar.baz"

  describe "encode/decode query" $ do
    it "is identity to encode and then decode" $
      property propEncodeDecodeQuery

    it "add ? in front of Query if and only if necessary" $
      property propQueryQuestionMark

  describe "decodePathSegments" $ do
    it "is inverse to encodePathSegments" $
      property $ \p -> not (p == [""]) ==> do
        (decodePathSegments . BL.toStrict . B.toLazyByteString . encodePathSegments) p `shouldBe` p

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

  describe "parseQuery" $ do
    it "returns value with '+' replaced to ' '" $ do
      parseQuery "?a=b+c+d" `shouldBe` [("a", Just "b c d")]

  describe "parseQueryReplacePlus" $ do
    it "returns value with '+' replaced to ' '" $ do
      parseQueryReplacePlus True "?a=b+c+d" `shouldBe` [("a", Just "b c d")]

    it "returns value with '+' preserved" $ do
      parseQueryReplacePlus False "?a=b+c+d" `shouldBe` [("a", Just "b+c+d")]
