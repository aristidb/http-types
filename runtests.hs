{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import Debug.Trace
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import qualified Data.Ascii               as A
import Network.HTTP.Types
import           Data.Text                   (Text)
import qualified Data.ByteString as S
import qualified Data.Text as T

main :: IO ()
main = defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "http-types"
    [ testProperty "encode/decode path" propEncodeDecodePath
    ]

propEncodeDecodePath :: ([Text], Query) -> Bool
propEncodeDecodePath (a, b) =
    let x = A.toByteString $ A.fromAsciiBuilder $ encodePath a b
        y = decodePath x
        z = y == (a, b)
     in if z then z else traceShow (a, b, x, y) z

instance Arbitrary Text where
    arbitrary = fmap T.pack arbitrary

instance Arbitrary S.ByteString where
    arbitrary = fmap S.pack arbitrary
