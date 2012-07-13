{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import           Data.Text                (Text)
import           Debug.Trace
import           Network.HTTP.Types
import           Test.Hspec
import           Test.QuickCheck
import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.ByteString          as S
import qualified Data.ByteString.Char8    as S8
import qualified Data.Text                as T

main :: IO ()
main = hspec $ do
    describe "encode/decode path" $ do
      it "is identity to encode and then decode"
        $ property propEncodeDecodePath
      it "does not escape period and dash" $
        Blaze.toByteString (encodePath ["foo-bar.baz"] []) `shouldBe` "/foo-bar.baz"

    describe "encode/decode query" $ do
      it "is identity to encode and then decode"
        $ property propEncodeDecodeQuery
      it "add ? in front of Query if and only if necessary"
        $ property propQueryQuestionMark

    describe "encode/decode path segments" $ do
      it "is identity to encode and then decode"
        $ property propEncodeDecodePathSegments

    describe "encode ByteRanges" $ do
      it "first 500 bytes" $ renderByteRanges [ByteRangeFromTo 0 499] `shouldBe` "bytes=0-499"
      it "second 500 bytes" $ renderByteRanges [ByteRangeFromTo 500 999] `shouldBe` "bytes=500-999"
      it "final 500 bytes" $ renderByteRanges [ByteRangeSuffix 500] `shouldBe` "bytes=-500"
      it "final 500 bytes (of 1000, absolute)" $ renderByteRanges [ByteRangeFrom 9500] `shouldBe` "bytes=9500-"
      it "first and last bytes only" $ renderByteRanges [ByteRangeFromTo 0 0, ByteRangeSuffix 1] `shouldBe` "bytes=0-0,-1"
      it "non-canonical second 500 bytes (1)" $ renderByteRanges [ByteRangeFromTo 500 600, ByteRangeFromTo 601 999] `shouldBe` "bytes=500-600,601-999"
      it "non-canonical second 500 bytes (2)" $ renderByteRanges [ByteRangeFromTo 500 700, ByteRangeFromTo 601 999] `shouldBe` "bytes=500-700,601-999"

propEncodeDecodePath :: ([Text], Query) -> Bool
propEncodeDecodePath (p', q') =
    let x = Blaze.toByteString $ encodePath a b
        y = decodePath x
        z = y == (a, b)
     in if z then z else traceShow (a, b, x, y) z
  where
    a = if p' == [""] then [] else p'
    b = filter (\(x, _) -> not (S.null x)) q'

propEncodeDecodeQuery :: Query -> Bool
propEncodeDecodeQuery q' =
    q == parseQuery (renderQuery True q)
  where
    q = filter (\(x, _) -> not (S.null x)) q'

propQueryQuestionMark :: (Bool, Query) -> Bool
propQueryQuestionMark (useQuestionMark, query) = actual == expected
    where
      actual = case S8.uncons $ renderQuery useQuestionMark query of
                 Nothing       -> False
                 Just ('?', _) -> True
                 _             -> False
      expected = case (useQuestionMark, null query) of
                   (False, _)    -> False
                   (True, True)  -> False
                   (True, False) -> True

propEncodeDecodePathSegments :: [Text] -> Bool
propEncodeDecodePathSegments p' =
    p == decodePathSegments (Blaze.toByteString $ encodePathSegments p)
  where
    p = if p' == [""] then [] else p'

instance Arbitrary Text where
    arbitrary = fmap T.pack arbitrary

instance Arbitrary S.ByteString where
    arbitrary = fmap S.pack arbitrary
