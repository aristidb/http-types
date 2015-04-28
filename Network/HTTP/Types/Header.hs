{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
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
import qualified Data.ByteString                as B
import qualified Data.ByteString.Builder        as BSB
import qualified Data.ByteString.Lazy           as BSL
import qualified Data.CaseInsensitive           as CI
import           Data.ByteString.Char8          () {-IsString-}
import           Data.Typeable                  (Typeable)
import           Data.Data                      (Data)

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
  deriving (Show, Eq, Ord, Typeable, Data)

renderByteRangeBuilder :: ByteRange -> BSB.Builder
renderByteRangeBuilder (ByteRangeFrom from) = BSB.integerDec from `mappend` BSB.char8 '-'
renderByteRangeBuilder (ByteRangeFromTo from to) = BSB.integerDec from `mappend` BSB.char8 '-' `mappend` BSB.integerDec to
renderByteRangeBuilder (ByteRangeSuffix suffix) = BSB.char8 '-' `mappend` BSB.integerDec suffix

renderByteRange :: ByteRange -> B.ByteString
renderByteRange = BSL.toStrict . BSB.toLazyByteString . renderByteRangeBuilder

-- | RFC 2616 Byte ranges (set).
type ByteRanges = [ByteRange]

renderByteRangesBuilder :: ByteRanges -> BSB.Builder
renderByteRangesBuilder xs = BSB.byteString "bytes=" `mappend` 
                             mconcat (intersperse (BSB.char8 ',') (map renderByteRangeBuilder xs))

renderByteRanges :: ByteRanges -> B.ByteString
renderByteRanges = BSL.toStrict . BSB.toLazyByteString . renderByteRangesBuilder
