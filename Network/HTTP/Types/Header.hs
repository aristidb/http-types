{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Type and constants for handling HTTP header fields.
--
-- At the bottom are also some functions to handle certain header field values.
module Network.HTTP.Types.Header (
    -- * HTTP Headers
    Header,
    HeaderName,
    RequestHeaders,
    ResponseHeaders,

    -- ** Common headers

    -- | The following header constants are provided for convenience,
    -- to prevent accidental spelling errors.
    hAccept,
    hAcceptCharset,
    hAcceptEncoding,
    hAcceptLanguage,
    hAcceptRanges,
    hAge,
    hAllow,
    hAuthorization,
    hCacheControl,
    hConnection,
    hContentDisposition,
    hContentEncoding,
    hContentLanguage,
    hContentLength,
    hContentLocation,
    hContentMD5,
    hContentRange,
    hContentType,
    hCookie,
    hDate,
    hETag,
    hExpect,
    hExpires,
    hFrom,
    hHost,
    hIfMatch,
    hIfModifiedSince,
    hIfNoneMatch,
    hIfRange,
    hIfUnmodifiedSince,
    hLastModified,
    hLocation,
    hMaxForwards,
    hMIMEVersion,
    hOrigin,
    hPragma,
    hPrefer,
    hPreferenceApplied,
    hProxyAuthenticate,
    hProxyAuthorization,
    hRange,
    hReferer,
    hRetryAfter,
    hServer,
    hSetCookie,
    hTE,
    hTrailer,
    hTransferEncoding,
    hUpgrade,
    hUserAgent,
    hVary,
    hVia,
    hWWWAuthenticate,
    hWarning,

    -- ** Byte ranges

    -- | Convenience functions and types to handle values from Range headers.
    --
    -- https://www.rfc-editor.org/rfc/rfc9110.html#name-byte-ranges
    ByteRange (..),
    renderByteRangeBuilder,
    renderByteRange,
    ByteRanges,
    renderByteRangesBuilder,
    renderByteRanges,
    parseByteRanges,
)
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.Data (Data)
import Data.List (intersperse)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | A full HTTP header field with the name and value separated.
--
-- E.g. @\"Content-Length: 28\"@ parsed into a 'Header' would turn into @("Content-Length", "28")@
type Header = (HeaderName, B.ByteString)

-- | A case-insensitive name of a header field.
--
-- This is the part of the header field before the colon: @HeaderName: some value@
type HeaderName = CI.CI B.ByteString

-- | A list of 'Header's.
--
-- Same type as 'ResponseHeaders', but useful to differentiate in type signatures.
type RequestHeaders = [Header]

-- | A list of 'Header's.
--
-- Same type as 'RequestHeaders', but useful to differentiate in type signatures.
type ResponseHeaders = [Header]

-- | [Accept](https://www.rfc-editor.org/rfc/rfc9110.html#name-accept)
--
-- @since 0.7.0
hAccept :: HeaderName
hAccept = "Accept"

-- | [Accept-Charset](https://www.rfc-editor.org/rfc/rfc9110.html#name-accept-charset)
--
-- @since 0.9
hAcceptCharset :: HeaderName
hAcceptCharset = "Accept-Charset"

-- | [Accept-Encoding](https://www.rfc-editor.org/rfc/rfc9110.html#name-accept-encoding)
--
-- @since 0.9
hAcceptEncoding :: HeaderName
hAcceptEncoding = "Accept-Encoding"

-- | [Accept-Language](https://www.rfc-editor.org/rfc/rfc9110.html#name-accept-language)
--
-- @since 0.7.0
hAcceptLanguage :: HeaderName
hAcceptLanguage = "Accept-Language"

-- | [Accept-Ranges](https://www.rfc-editor.org/rfc/rfc9110.html#name-accept-ranges)
--
-- @since 0.9
hAcceptRanges :: HeaderName
hAcceptRanges = "Accept-Ranges"

-- | [Age](https://www.rfc-editor.org/rfc/rfc9111.html#name-age)
--
-- @since 0.9
hAge :: HeaderName
hAge = "Age"

-- | [Allow](https://www.rfc-editor.org/rfc/rfc9110.html#name-allow)
--
-- @since 0.9
hAllow :: HeaderName
hAllow = "Allow"

-- | [Authorization](https://www.rfc-editor.org/rfc/rfc9110.html#name-authorization)
--
-- @since 0.7.0
hAuthorization :: HeaderName
hAuthorization = "Authorization"

-- | [Cache-Control](https://www.rfc-editor.org/rfc/rfc9111.html#name-cache-control)
--
-- @since 0.7.0
hCacheControl :: HeaderName
hCacheControl = "Cache-Control"

-- | [Connection](https://www.rfc-editor.org/rfc/rfc9110.html#name-connection)
--
-- @since 0.7.0
hConnection :: HeaderName
hConnection = "Connection"

-- | [Content-Encoding](https://www.rfc-editor.org/rfc/rfc9110.html#name-content-encoding)
--
-- @since 0.7.0
hContentEncoding :: HeaderName
hContentEncoding = "Content-Encoding"

-- | [Content-Language](https://www.rfc-editor.org/rfc/rfc9110.html#name-content-language)
--
-- @since 0.9
hContentLanguage :: HeaderName
hContentLanguage = "Content-Language"

-- | [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#name-content-length)
--
-- @since 0.7.0
hContentLength :: HeaderName
hContentLength = "Content-Length"

-- | [Content-Location](https://www.rfc-editor.org/rfc/rfc9110.html#name-content-location)
--
-- @since 0.9
hContentLocation :: HeaderName
hContentLocation = "Content-Location"

-- | [Content-MD5](https://www.rfc-editor.org/rfc/rfc2616.html#section-14.15)
--
-- /This header has been obsoleted in RFC 9110./
--
-- @since 0.7.0
hContentMD5 :: HeaderName
hContentMD5 = "Content-MD5"

-- | [Content-Range](https://www.rfc-editor.org/rfc/rfc9110.html#name-content-range)
--
-- @since 0.9
hContentRange :: HeaderName
hContentRange = "Content-Range"

-- | [Content-Type](https://www.rfc-editor.org/rfc/rfc9110.html#name-content-type)
--
-- @since 0.7.0
hContentType :: HeaderName
hContentType = "Content-Type"

-- | [Date](https://www.rfc-editor.org/rfc/rfc9110.html#name-date)
--
-- @since 0.7.0
hDate :: HeaderName
hDate = "Date"

-- | [ETag](https://www.rfc-editor.org/rfc/rfc9110.html#name-etag)
--
-- @since 0.9
hETag :: HeaderName
hETag = "ETag"

-- | [Expect](https://www.rfc-editor.org/rfc/rfc9110.html#name-expect)
--
-- @since 0.9
hExpect :: HeaderName
hExpect = "Expect"

-- | [Expires](https://www.rfc-editor.org/rfc/rfc9111.html#name-expires)
--
-- @since 0.9
hExpires :: HeaderName
hExpires = "Expires"

-- | [From](https://www.rfc-editor.org/rfc/rfc9110.html#name-from)
--
-- @since 0.9
hFrom :: HeaderName
hFrom = "From"

-- | [Host](https://www.rfc-editor.org/rfc/rfc9110.html#name-host-and-authority)
--
-- @since 0.9
hHost :: HeaderName
hHost = "Host"

-- | [If-Match](https://www.rfc-editor.org/rfc/rfc9110.html#name-if-match)
--
-- @since 0.9
hIfMatch :: HeaderName
hIfMatch = "If-Match"

-- | [If-Modified-Since](https://www.rfc-editor.org/rfc/rfc9110.html#name-if-modified-since)
--
-- @since 0.7.0
hIfModifiedSince :: HeaderName
hIfModifiedSince = "If-Modified-Since"

-- | [If-None-Match](https://www.rfc-editor.org/rfc/rfc9110.html#name-if-none-match)
--
-- @since 0.9
hIfNoneMatch :: HeaderName
hIfNoneMatch = "If-None-Match"

-- | [If-Range](https://www.rfc-editor.org/rfc/rfc9110.html#name-if-range)
--
-- @since 0.7.0
hIfRange :: HeaderName
hIfRange = "If-Range"

-- | [If-Unmodified-Since](https://www.rfc-editor.org/rfc/rfc9110.html#name-if-unmodified-since)
--
-- @since 0.9
hIfUnmodifiedSince :: HeaderName
hIfUnmodifiedSince = "If-Unmodified-Since"

-- | [Last-Modified](https://www.rfc-editor.org/rfc/rfc9110.html#name-last-modified)
--
-- @since 0.7.0
hLastModified :: HeaderName
hLastModified = "Last-Modified"

-- | [Location](https://www.rfc-editor.org/rfc/rfc9110.html#name-location)
--
-- @since 0.7.1
hLocation :: HeaderName
hLocation = "Location"

-- | [Max-Forwards](https://www.rfc-editor.org/rfc/rfc9110.html#name-max-forwards)
--
-- @since 0.9
hMaxForwards :: HeaderName
hMaxForwards = "Max-Forwards"

-- | [Pragma](https://www.rfc-editor.org/rfc/rfc9111.html#name-pragma)
--
-- /This header has been deprecated in RFC 9111 in favor of "Cache-Control"./
--
-- @since 0.9
hPragma :: HeaderName
hPragma = "Pragma"

-- | [Proxy-Authenticate](https://www.rfc-editor.org/rfc/rfc9110.html#name-proxy-authenticate)
--
-- @since 0.9
hProxyAuthenticate :: HeaderName
hProxyAuthenticate = "Proxy-Authenticate"

-- | [Proxy-Authorization](https://www.rfc-editor.org/rfc/rfc9110.html#name-proxy-authorization)
--
-- @since 0.9
hProxyAuthorization :: HeaderName
hProxyAuthorization = "Proxy-Authorization"

-- | [Range](https://www.rfc-editor.org/rfc/rfc9110.html#name-range)
--
-- @since 0.7.0
hRange :: HeaderName
hRange = "Range"

-- | [Referer](https://www.rfc-editor.org/rfc/rfc9110.html#name-referer)
--
-- @since 0.7.0
hReferer :: HeaderName
hReferer = "Referer"

-- | [Retry-After](https://www.rfc-editor.org/rfc/rfc9110.html#name-retry-after)
--
-- @since 0.9
hRetryAfter :: HeaderName
hRetryAfter = "Retry-After"

-- | [Server](https://www.rfc-editor.org/rfc/rfc9110.html#name-server)
--
-- @since 0.7.1
hServer :: HeaderName
hServer = "Server"

-- | [TE](https://www.rfc-editor.org/rfc/rfc9110.html#name-te)
--
-- @since 0.9
hTE :: HeaderName
hTE = "TE"

-- | [Trailer](https://www.rfc-editor.org/rfc/rfc9110.html#name-trailer)
--
-- @since 0.9
hTrailer :: HeaderName
hTrailer = "Trailer"

-- | [Transfer-Encoding](https://www.rfc-editor.org/rfc/rfc9112#name-transfer-encoding)
--
-- @since 0.9
hTransferEncoding :: HeaderName
hTransferEncoding = "Transfer-Encoding"

-- | [Upgrade](https://www.rfc-editor.org/rfc/rfc9110.html#name-upgrade)
--
-- @since 0.9
hUpgrade :: HeaderName
hUpgrade = "Upgrade"

-- | [User-Agent](https://www.rfc-editor.org/rfc/rfc9110.html#name-user-agent)
--
-- @since 0.7.0
hUserAgent :: HeaderName
hUserAgent = "User-Agent"

-- | [Vary](https://www.rfc-editor.org/rfc/rfc9110.html#name-vary)
--
-- @since 0.9
hVary :: HeaderName
hVary = "Vary"

-- | [Via](https://www.rfc-editor.org/rfc/rfc9110.html#name-via)
--
-- @since 0.9
hVia :: HeaderName
hVia = "Via"

-- | [WWW-Authenticate](https://www.rfc-editor.org/rfc/rfc9110.html#name-www-authenticate)
--
-- @since 0.9
hWWWAuthenticate :: HeaderName
hWWWAuthenticate = "WWW-Authenticate"

-- | [Warning](https://www.rfc-editor.org/rfc/rfc9111.html#name-warning)
--
-- /This header has been obsoleted in RFC 9110./
--
-- @since 0.9
hWarning :: HeaderName
hWarning = "Warning"

-- | [Content-Disposition](https://www.rfc-editor.org/rfc/rfc6266.html)
--
-- @since 0.10
hContentDisposition :: HeaderName
hContentDisposition = "Content-Disposition"

-- | [MIME-Version](https://www.rfc-editor.org/rfc/rfc2616.html#section-19.4.1)
--
-- @since 0.10
hMIMEVersion :: HeaderName
hMIMEVersion = "MIME-Version"

-- | [Cookie](https://www.rfc-editor.org/rfc/rfc6265.html#section-4.2)
--
-- @since 0.7.0
hCookie :: HeaderName
hCookie = "Cookie"

-- | [Set-Cookie](https://www.rfc-editor.org/rfc/rfc6265.html#section-4.1)
--
-- @since 0.10
hSetCookie :: HeaderName
hSetCookie = "Set-Cookie"

-- | [Origin](https://www.rfc-editor.org/rfc/rfc6454.html#section-7)
--
-- @since 0.10
hOrigin :: HeaderName
hOrigin = "Origin"

-- | [Prefer](https://www.rfc-editor.org/rfc/rfc7240.html#section-2)
--
-- @since 0.12.2
hPrefer :: HeaderName
hPrefer = "Prefer"

-- | [Preference-Applied](https://www.rfc-editor.org/rfc/rfc7240.html#section-3)
--
-- @since 0.12.2
hPreferenceApplied :: HeaderName
hPreferenceApplied = "Preference-Applied"

-- | An individual byte range.
--
-- Negative indices are not allowed!
--
-- @since 0.6.11
data ByteRange
    = ByteRangeFrom !Integer
    | ByteRangeFromTo !Integer !Integer
    | ByteRangeSuffix !Integer
    deriving
        ( -- | @since 0.8.4
          Show
        , -- | @since 0.8.4
          Eq
        , -- | @since 0.8.4
          Ord
        , -- | @since 0.8.4
          Typeable
        , -- | @since 0.8.4
          Data
        , -- | @since 0.12.4
          Generic
        )

-- | Turns a byte range into a byte string 'B.Builder'.
--
-- @since 0.6.11
renderByteRangeBuilder :: ByteRange -> B.Builder
renderByteRangeBuilder (ByteRangeFrom from) = B.integerDec from `mappend` B.char7 '-'
renderByteRangeBuilder (ByteRangeFromTo from to) = B.integerDec from `mappend` B.char7 '-' `mappend` B.integerDec to
renderByteRangeBuilder (ByteRangeSuffix suffix) = B.char7 '-' `mappend` B.integerDec suffix

-- | Renders a byte range into a 'B.ByteString'.
--
-- >>> renderByteRange (ByteRangeFrom 2048)
-- "2048-"
--
-- @since 0.6.11
renderByteRange :: ByteRange -> B.ByteString
renderByteRange = BL.toStrict . B.toLazyByteString . renderByteRangeBuilder

-- | A list of byte ranges.
--
-- @since 0.6.11
type ByteRanges = [ByteRange]

-- | Turns a list of byte ranges into a byte string 'B.Builder'.
--
-- @since 0.6.11
renderByteRangesBuilder :: ByteRanges -> B.Builder
renderByteRangesBuilder xs =
    B.byteString "bytes="
        `mappend` mconcat (intersperse (B.char7 ',') $ map renderByteRangeBuilder xs)

-- | Renders a list of byte ranges into a 'B.ByteString'.
--
-- >>> renderByteRanges [ByteRangeFrom 2048, ByteRangeSuffix 20]
-- "bytes=2048-,-20"
--
-- @since 0.6.11
renderByteRanges :: ByteRanges -> B.ByteString
renderByteRanges = BL.toStrict . B.toLazyByteString . renderByteRangesBuilder

-- | Parse the value of a Range header into a 'ByteRanges'.
--
-- >>> parseByteRanges "error"
-- Nothing
-- >>> parseByteRanges "bytes=0-499"
-- Just [ByteRangeFromTo 0 499]
-- >>> parseByteRanges "bytes=500-999"
-- Just [ByteRangeFromTo 500 999]
-- >>> parseByteRanges "bytes=-500"
-- Just [ByteRangeSuffix 500]
-- >>> parseByteRanges "bytes=9500-"
-- Just [ByteRangeFrom 9500]
-- >>> parseByteRanges "bytes=0-0,-1"
-- Just [ByteRangeFromTo 0 0,ByteRangeSuffix 1]
-- >>> parseByteRanges "bytes=500-600,601-999"
-- Just [ByteRangeFromTo 500 600,ByteRangeFromTo 601 999]
-- >>> parseByteRanges "bytes=500-700,601-999"
-- Just [ByteRangeFromTo 500 700,ByteRangeFromTo 601 999]
--
-- @since 0.9.1
parseByteRanges :: B.ByteString -> Maybe ByteRanges
parseByteRanges bs1 = do
    bs2 <- stripPrefixB "bytes=" bs1
    (r, bs3) <- range bs2
    ranges (r :) bs3
  where
    range bs2 = do
        (i, bs3) <- B8.readInteger bs2
        if i < 0 -- has prefix "-" ("-0" is not valid, but here treated as "0-")
            then Just (ByteRangeSuffix (negate i), bs3)
            else do
                bs4 <- stripPrefixB "-" bs3
                case B8.readInteger bs4 of
                    Just (j, bs5) | j >= i -> Just (ByteRangeFromTo i j, bs5)
                    _ -> Just (ByteRangeFrom i, bs4)
    ranges front bs3
        | B.null bs3 = Just (front [])
        | otherwise = do
            bs4 <- stripPrefixB "," bs3
            (r, bs5) <- range bs4
            ranges (front . (r :)) bs5

    -- FIXME: Use 'stripPrefix' from the 'bytestring' package.
    -- Might have to update the dependency constraints though.
    stripPrefixB x y
        | x `B.isPrefixOf` y = Just (B.drop (B.length x) y)
        | otherwise = Nothing
