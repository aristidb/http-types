{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Debug.Trace
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary (..))
import qualified Data.Ascii               as A
import Network.HTTP.Types
import           Data.Text                   (Text)
import qualified Data.ByteString as S
import qualified Data.Text as T

main :: IO ()
main = hspec $ descriptions
    [ describe "encode/decode path"
        [ it "is identity to encode and then decode"
            $ property propEncodeDecodePath
        ]
    , describe "encode/decode query"
        [ it "is identity to encode and then decode"
            $ property propEncodeDecodeQuery
        ]
    , describe "encode/decode path segments"
        [ it "is identity to encode and then decode"
            $ property propEncodeDecodePathSegments
        ]
    ]

propEncodeDecodePath :: ([Text], Query) -> Bool
propEncodeDecodePath (p', q') =
    let x = A.toByteString $ A.fromAsciiBuilder $ encodePath a b
        y = decodePath x
        z = y == (a, b)
     in if z then z else traceShow (a, b, x, y) z
  where
    a = if p' == [""] then [] else p'
    b = filter (\(x, _) -> not (S.null x)) q'

propEncodeDecodeQuery :: Query -> Bool
propEncodeDecodeQuery q' =
    q == parseQuery (A.toByteString $ renderQuery True q)
  where
    q = filter (\(x, _) -> not (S.null x)) q'

propEncodeDecodePathSegments :: [Text] -> Bool
propEncodeDecodePathSegments p' =
    p == decodePathSegments (A.toByteString $ A.fromAsciiBuilder $ encodePathSegments p)
  where
    p = if p' == [""] then [] else p'

instance Arbitrary Text where
    arbitrary = fmap T.pack arbitrary

instance Arbitrary S.ByteString where
    arbitrary = fmap S.pack arbitrary
