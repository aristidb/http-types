{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import           Data.Text                (Text)
import           Debug.Trace
import           Network.HTTP.Types
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.HUnit
import           Test.QuickCheck          (Arbitrary (..))
import           Test.HUnit
import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.ByteString          as S
import qualified Data.ByteString.Char8    as S8
import qualified Data.Text                as T

--main :: IO ()
main = hspecX
    [ describe "encode/decode path"
        [ it "is identity to encode and then decode"
            $ property propEncodeDecodePath
        , it "does not escape period and dash" $
            Blaze.toByteString (encodePath ["foo-bar.baz"] []) @?= "/foo-bar.baz"
        ]
    , describe "encode/decode query"
        [ it "is identity to encode and then decode"
            $ property propEncodeDecodeQuery
        , it "add ? in front of Query if and only if necessary"
            $ property propQueryQuestionMark
        ]
    , describe "encode/decode path segments"
        [ it "is identity to encode and then decode"
            $ property propEncodeDecodePathSegments
        ]
    ]

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
