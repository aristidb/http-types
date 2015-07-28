{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, CPP #-}
module Network.HTTP.Types.Header
(
  -- ** Types
  Header
, HeaderName
, RequestHeaders
, ResponseHeaders
  -- ** Common headers
, hAccept
, hAcceptCharset
, hAcceptEncoding
, hAcceptLanguage
, hAcceptRanges
, hAge
, hAllow
, hAuthorization
, hCacheControl
, hConnection
, hContentEncoding
, hContentLanguage
, hContentLength
, hContentLocation
, hContentMD5
, hContentRange
, hContentType
, hCookie
, hDate
, hETag
, hExpect
, hExpires
, hFrom
, hHost
, hIfMatch
, hIfModifiedSince
, hIfNoneMatch
, hIfRange
, hIfUnmodifiedSince
, hLastModified
, hLocation
, hMaxForwards
, hPragma
, hProxyAuthenticate
, hProxyAuthorization
, hRange
, hReferer
, hRetryAfter
, hServer
, hTE
, hTrailer
, hTransferEncoding
, hUpgrade
, hUserAgent
, hVary
, hVia
, hWWWAuthenticate
, hWarning
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
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid
#endif
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze
import qualified Data.ByteString                as B
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
-- According to http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
hAccept, hAcceptCharset, hAcceptEncoding, hAcceptLanguage, hAcceptRanges, hAge, hAllow, hAuthorization, hCacheControl, hConnection, hContentEncoding, hContentLanguage, hContentLength, hContentLocation, hContentMD5, hContentRange, hContentType, hCookie, hDate, hETag, hExpect, hExpires, hFrom, hHost, hIfMatch, hIfModifiedSince, hIfNoneMatch, hIfRange, hIfUnmodifiedSince, hLastModified, hLocation, hMaxForwards, hPragma, hProxyAuthenticate, hProxyAuthorization, hRange, hReferer, hRetryAfter, hServer, hTE, hTrailer, hTransferEncoding, hUpgrade, hUserAgent, hVary, hVia, hWWWAuthenticate, hWarning :: HeaderName
hAccept             = "Accept"
hAcceptCharset      = "Accept-Charset"
hAcceptEncoding     = "Accept-Encoding"
hAcceptLanguage     = "Accept-Language"
hAcceptRanges       = "Accept-Ranges"
hAge                = "Age"
hAllow              = "Allow"
hAuthorization      = "Authorization"
hCacheControl       = "Cache-Control"
hConnection         = "Connection"
hContentEncoding    = "Content-Encoding"
hContentLanguage    = "Content-Language"
hContentLength      = "Content-Length"
hContentLocation    = "Content-Location"
hContentMD5         = "Content-MD5"
hContentRange       = "Content-Range"
hContentType        = "Content-Type"
hCookie             = "Cookie"
hDate               = "Date"
hETag               = "ETag"
hExpect             = "Expect"
hExpires            = "Expires"
hFrom               = "From"
hHost               = "Host"
hIfMatch            = "If-Match"
hIfModifiedSince    = "If-Modified-Since"
hIfNoneMatch        = "If-None-Match"
hIfRange            = "If-Range"
hIfUnmodifiedSince  = "If-Unmodified-Since"
hLastModified       = "Last-Modified"
hLocation           = "Location"
hMaxForwards        = "Max-Forwards"
hPragma             = "Pragma"
hProxyAuthenticate  = "Proxy-Authenticate"
hProxyAuthorization = "Proxy-Authorization"
hRange              = "Range"
hReferer            = "Referer"
hRetryAfter         = "Retry-After"
hServer             = "Server"
hTE                 = "TE"
hTrailer            = "Trailer"
hTransferEncoding   = "Transfer-Encoding"
hUpgrade            = "Upgrade"
hUserAgent          = "User-Agent"
hVary               = "Vary"
hVia                = "Via"
hWWWAuthenticate    = "WWW-Authenticate"
hWarning            = "Warning"

-- | RFC 2616 Byte range (individual).
--
-- Negative indices are not allowed!
data ByteRange
  = ByteRangeFrom !Integer
  | ByteRangeFromTo !Integer !Integer
  | ByteRangeSuffix !Integer
  deriving (Show, Eq, Ord, Typeable, Data)

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
