{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Types.Header
(
  -- ** Types
  Header
, RequestHeaders
, ResponseHeaders
  -- ** Common headers
, headerAccept
, headerAuthorization
, headerCacheControl
, headerConnection
, headerContentLength
, headerContentType
, headerContentMD5
, headerDate
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
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze
import qualified Data.ByteString                as B
import qualified Data.CaseInsensitive           as CI
import           Data.ByteString.Char8          () {-IsString-}

-- | Header
type Header = (CI.CI B.ByteString, B.ByteString)

-- | Request Headers
type RequestHeaders = [Header]

-- | Response Headers
type ResponseHeaders = [Header]

-- | HTTP Headers
headerAccept, headerAuthorization, headerCacheControl, headerConnection, headerContentLength, headerContentType, headerContentMD5, headerDate :: B.ByteString -> Header
headerAccept        = (,) "Accept"
headerAuthorization = (,) "Authorization"
headerCacheControl  = (,) "Cache-Control"
headerConnection    = (,) "Connection"
headerContentLength = (,) "Content-Length"
headerContentType   = (,) "Content-Type"
headerContentMD5    = (,) "Content-MD5"
headerDate          = (,) "Date"

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
