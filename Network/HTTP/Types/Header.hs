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
  -- ** Deprecated
, headerAccept
, headerAuthorization
, headerCacheControl
, headerConnection
, headerContentLength
, headerContentType
, headerContentMD5
, headerDate
)
where

import           Data.List
import           Data.Monoid
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze
import qualified Data.ByteString                as B
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

-- | HTTP Headers
headerAccept, headerAuthorization, headerCacheControl, headerConnection, headerContentLength, headerContentType, headerContentMD5, headerDate :: B.ByteString -> Header
headerAccept        = (,) hAccept
{-# DEPRECATED headerAccept "Use hAccept instead" #-}
headerAuthorization = (,) hAuthorization
{-# DEPRECATED headerAuthorization "Use hAuthorization instead" #-}
headerCacheControl  = (,) hCacheControl
{-# DEPRECATED headerCacheControl "Use hCacheControl instead" #-}
headerConnection    = (,) hConnection
{-# DEPRECATED headerConnection "Use hConnection instead" #-}
headerContentLength = (,) hContentLength
{-# DEPRECATED headerContentLength "Use hContentLength instead" #-}
headerContentMD5    = (,) hContentMD5
{-# DEPRECATED headerContentMD5 "Use hContentMD5 instead" #-}
headerContentType   = (,) hContentType
{-# DEPRECATED headerContentType "Use hContentType instead" #-}
headerDate          = (,) hDate
{-# DEPRECATED headerDate "Use hDate instead" #-}

-- | RFC 2616 Byte range (individual). 
-- 
-- Negative indices are not allowed!
data ByteRange 
  = ByteRangeFrom !Integer
  | ByteRangeFromTo !Integer !Integer
  | ByteRangeSuffix !Integer

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
