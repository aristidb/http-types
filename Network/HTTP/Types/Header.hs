{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Network.HTTP.Types.Header
(
  -- ** Types
  Header
, HeaderName
, RequestHeaders
, ResponseHeaders
  -- ** Common headers
, hAccept
, hAcceptLanguage
, hAuthorization
, hCacheControl
, hCookie
, hConnection
, hContentEncoding
, hContentLength
, hContentMD5
, hContentType
, hDate
, hIfModifiedSince
, hIfRange
, hLastModified
, hLocation
, hRange
, hReferer
, hServer
, hUserAgent
  -- ** Byte ranges
, ByteRange(..)
, renderByteRangeBuilder
, renderByteRange
, ByteRanges
, renderByteRangesBuilder
, renderByteRanges
, parseByteRanges
)
where

import           Data.List
import           Data.Monoid
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze
import qualified Data.ByteString                as B
import qualified Data.CaseInsensitive           as CI
import           Data.ByteString.Char8          () {-IsString-}
import qualified Data.Attoparsec                as A
import qualified Data.Attoparsec.ByteString.Char8  as A8
import           Control.Applicative ((<|>), (<$>))
-- | Header
type Header = (HeaderName, B.ByteString)

-- | Header name
type HeaderName = CI.CI B.ByteString

-- | Request Headers
type RequestHeaders = [Header]

-- | Response Headers
type ResponseHeaders = [Header]

-- | HTTP Header names
hAccept, hAcceptLanguage, hAuthorization, hCacheControl, hConnection, hContentEncoding, hContentLength, hContentMD5, hContentType, hCookie, hDate, hIfModifiedSince, hIfRange, hLastModified, hLocation, hRange, hReferer, hServer, hUserAgent :: HeaderName
hAccept          = "Accept"
hAcceptLanguage  = "Accept-Language"
hAuthorization   = "Authorization"
hCacheControl    = "Cache-Control"
hConnection      = "Connection"
hContentEncoding = "Content-Encoding"
hContentLength   = "Content-Length"
hContentMD5      = "Content-MD5"
hContentType     = "Content-Type"
hCookie          = "Cookie"
hDate            = "Date"
hIfModifiedSince = "If-Modified-Since"
hIfRange         = "If-Range"
hLastModified    = "Last-Modified"
hLocation        = "Location"
hRange           = "Range"
hReferer         = "Referer"
hServer          = "Server"
hUserAgent       = "User-Agent"

-- | RFC 2616 Byte range (individual). 
-- 
-- Negative indices are not allowed!
data ByteRange 
  = ByteRangeFrom !Integer
  | ByteRangeFromTo !Integer !Integer
  | ByteRangeSuffix !Integer
  deriving (Show)

renderByteRangeBuilder :: ByteRange -> Blaze.Builder
renderByteRangeBuilder (ByteRangeFrom from) = Blaze.fromShow from `mappend` Blaze.fromChar '-'
renderByteRangeBuilder (ByteRangeFromTo from to) = Blaze.fromShow from `mappend` Blaze.fromChar '-' `mappend` Blaze.fromShow to
renderByteRangeBuilder (ByteRangeSuffix suffix) = Blaze.fromChar '-' `mappend` Blaze.fromShow suffix

renderByteRange :: ByteRange -> B.ByteString
renderByteRange = Blaze.toByteString . renderByteRangeBuilder

-- | RFC 2616 Byte ranges (set).
type ByteRanges = [ByteRange]

renderByteRangesBuilder :: ByteRanges -> Blaze.Builder
renderByteRangesBuilder xs = Blaze.copyByteString "bytes=" `mappend` 
                             mconcat (intersperse (Blaze.fromChar ',') (map renderByteRangeBuilder xs))

renderByteRanges :: ByteRanges -> B.ByteString
renderByteRanges = Blaze.toByteString . renderByteRangesBuilder


byteRangesParser :: A8.Parser ByteRanges
byteRangesParser = do
    _ <- A8.string "bytes="
    br <- range
    rest <- maybeMoreRanges 
    return $ br:rest
    where
        range = rangeFromTo <|> rangeSuffix
        rangeFromTo = do
            f <- A8.decimal
            _ <- A8.char '-'
            mt <- Just <$> A8.decimal <|> return Nothing
            
            return $ case mt of
                Just t -> ByteRangeFromTo f t
                Nothing -> ByteRangeFrom f
        rangeSuffix = do
            _ <- A8.char '-'
            s <- A8.decimal
            return $ ByteRangeSuffix s
        maybeMoreRanges = moreRanges <|> return []
        moreRanges = do
            _ <- A8.char ','
            r <- range
            rest <- maybeMoreRanges
            return $ r:rest
       
parseByteRanges :: B.ByteString -> Maybe ByteRanges
parseByteRanges bs = case A8.parseOnly byteRangesParser bs of
    Left _ -> Nothing
    Right br -> Just br
