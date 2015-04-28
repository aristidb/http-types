module Main (main) where

import Network.HTTP.Types
import Criterion.Main
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS

main = defaultMain benchmarks

toByteString :: BSB.Builder -> BS.ByteString
toByteString = BSL.toStrict . BSB.toLazyByteString

simpleTest = BS.pack "hello world"
complexTest = BS.pack "hello@w:w+orld!"

benchmarks =
    [ bench "QS simple" $ nf (toByteString . urlEncodeBuilder False)  simpleTest
    , bench "PI simple" $ nf (toByteString . urlEncodeBuilder True )  simpleTest
    , bench "QS complex" $ nf (toByteString . urlEncodeBuilder False) complexTest
    , bench "PI complex" $ nf (toByteString . urlEncodeBuilder True ) complexTest
    ]
