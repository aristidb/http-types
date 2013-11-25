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
)
where

import           Data.List
import           Data.Monoid
import qualified Data.ByteString.Lazy.Builder   as B
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import qualified Data.CaseInsensitive           as CI
import           Data.ByteString.Char8          () {-IsString-}

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

fromShow :: Show a => a -> B.Builder
fromShow a = mconcat $ map B.char8 $ show a

renderByteRangeBuilder :: ByteRange -> B.Builder
renderByteRangeBuilder (ByteRangeFrom from) = fromShow from `mappend` B.char8 '-'
renderByteRangeBuilder (ByteRangeFromTo from to) = fromShow from `mappend` B.char8 '-' `mappend` fromShow to
renderByteRangeBuilder (ByteRangeSuffix suffix) = B.char8 '-' `mappend` fromShow suffix

renderByteRange :: ByteRange -> B.ByteString
renderByteRange = L.toStrict . B.toLazyByteString . renderByteRangeBuilder

-- | RFC 2616 Byte ranges (set).
type ByteRanges = [ByteRange]

renderByteRangesBuilder :: ByteRanges -> B.Builder
renderByteRangesBuilder xs = B.byteString "bytes=" `mappend` 
                             mconcat (intersperse (B.char8 ',') (map renderByteRangeBuilder xs))

renderByteRanges :: ByteRanges -> B.ByteString
renderByteRanges = L.toStrict . B.toLazyByteString . renderByteRangesBuilder
