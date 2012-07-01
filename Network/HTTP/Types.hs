{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Types
(
  -- * General
  Ascii
  -- * Methods
, Method
, methodGet
, methodPost
, methodHead
, methodPut
, methodDelete
, methodTrace
, methodConnect
, methodOptions
, StdMethod(..)
, parseMethod
, renderMethod
, renderStdMethod
  -- * Versions
, HttpVersion(..)
, http09
, http10
, http11
  -- * Status
, Status(..)
, status100
, continue100
, status101
, switchingProtocols101
, status200
, ok200
, status201
, created201
, status202
, accepted202
, status203
, nonAuthoritative203
, status204
, noContent204
, status205
, resetContent205
, status206
, partialContent206
, status300
, multipleChoices300
, status301
, movedPermanently301
, status302
, found302
, status303
, seeOther303
, status304
, notModified304
, status305
, useProxy305
, status307
, temporaryRedirect307
, status400
, badRequest400
, status401
, unauthorized401
, status402
, paymentRequired402
, status403
, forbidden403
, status404
, notFound404
, status405
, methodNotAllowed405
, status406
, notAcceptable406
, status407
, proxyAuthenticationRequired407
, status408
, requestTimeout408
, status409
, conflict409
, status410
, gone410
, status411
, lengthRequired411
, status412
, preconditionFailed412
, status413
, requestEntityTooLarge413
, status414
, requestURITooLong414
, status415
, unsupportedMediaType415
, status416
, requestedRangeNotSatisfiable416
, status417
, expectationFailed417
, status418
, imATeaPot418
, status500
, internalServerError500
, status501
, notImplemented501
, status502
, badGateway502
, status503
, serviceUnavailable503
, status504
, gatewayTimeout504
, status505
, httpVersionNotSupported505
  -- * Headers
  -- ** Types
, Header
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
  -- * Query string
, QueryItem
, Query
, SimpleQueryItem
, SimpleQuery
, simpleQueryToQuery
, renderQuery
, renderQueryBuilder
, renderSimpleQuery
, parseQuery
, parseSimpleQuery
  -- ** Text query string (UTF8 encoded)
, QueryText
, queryTextToQuery
, queryToQueryText
, renderQueryText
, parseQueryText
  -- * Path segments
, encodePathSegments
, decodePathSegments
, encodePathSegmentsRelative
  -- * Path (segments + query string)
, encodePath
, decodePath
  -- * URL encoding / decoding
, urlEncodeBuilder
, urlEncode
, urlDecode
)
where

import           Control.Arrow                  (second, (|||), (***))
import           Data.Array
import           Data.Bits                      (shiftL, (.|.))
import           Data.Char
import           Data.Maybe
import           Data.Monoid                    (mempty, mappend, mconcat)
import           Data.Text                      (Text)
import           Data.Text.Encoding             (encodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error       (lenientDecode)
import           Data.Word                      (Word8)
import           Data.List                      (intersperse)
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.CaseInsensitive           as CI

type Ascii = B.ByteString

-- | HTTP method (flat string type).
type Method = Ascii

-- | HTTP Method constants.
methodGet, methodPost, methodHead, methodPut, methodDelete, methodTrace, methodConnect, methodOptions :: Method
methodGet     = renderStdMethod GET
methodPost    = renderStdMethod POST
methodHead    = renderStdMethod HEAD
methodPut     = renderStdMethod PUT
methodDelete  = renderStdMethod DELETE
methodTrace   = renderStdMethod TRACE
methodConnect = renderStdMethod CONNECT
methodOptions = renderStdMethod OPTIONS

-- | HTTP standard method (as defined by RFC 2616).
data StdMethod
    = GET
    | POST
    | HEAD  
    | PUT
    | DELETE
    | TRACE
    | CONNECT
    | OPTIONS
    deriving (Read, Show, Eq, Ord, Enum, Bounded, Ix)
-- These are ordered by suspected frequency. More popular methods should go first.
-- The reason is that methodList is used with lookup.
-- lookup is probably faster for these few cases than setting up an elaborate data structure.

methodArray :: Array StdMethod Method
methodArray = listArray (minBound, maxBound) $ map (B8.pack . show) [minBound :: StdMethod .. maxBound]

methodList :: [(Method, StdMethod)]
methodList = map (\(a, b) -> (b, a)) (assocs methodArray)

-- | Convert a method 'ByteString' to a 'StdMethod' if possible.
parseMethod :: Method -> Either Ascii StdMethod
parseMethod bs = maybe (Left bs) Right $ lookup bs methodList

-- | Convert an algebraic method to a 'ByteString'.
renderMethod :: Either Ascii StdMethod -> Method
renderMethod = id ||| renderStdMethod

-- | Convert a 'StdMethod' to a 'ByteString'.
renderStdMethod :: StdMethod -> Method
renderStdMethod m = methodArray ! m

-- | HTTP Version.
-- 
-- Note that the Show instance is intended merely for debugging.
data HttpVersion 
    = HttpVersion {
        httpMajor :: !Int 
      , httpMinor :: !Int
      }
    deriving (Eq, Ord)

instance Show HttpVersion where
    show (HttpVersion major minor) = "HTTP/" ++ show major ++ "." ++ show minor

-- | HTTP 0.9
http09 :: HttpVersion
http09 = HttpVersion 0 9

-- | HTTP 1.0
http10 :: HttpVersion
http10 = HttpVersion 1 0

-- | HTTP 1.1
http11 :: HttpVersion
http11 = HttpVersion 1 1

-- | HTTP Status.
-- 
-- Only the 'statusCode' is used for comparisons.
-- 
-- Note that the Show instance is only for debugging.
data Status
    = Status {
        statusCode :: Int
      , statusMessage :: Ascii
      }
    deriving (Show)

instance Eq Status where
    Status { statusCode = a } == Status { statusCode = b } = a == b

instance Ord Status where
    compare Status { statusCode = a } Status { statusCode = b } = a `compare` b

-- | Continue 100
status100 :: Status
status100 = Status 100 "Continue"

-- | Continue 100
continue100 :: Status
continue100 = status100

-- | Switching Protocols 101
status101 :: Status
status101 = Status 101 "Switching Protocols"

-- | Switching Protocols 101
switchingProtocols101 :: Status
switchingProtocols101 = status101

-- | OK 200
status200 :: Status
status200 = Status 200 "OK"

-- | OK 200
ok200 :: Status
ok200 = status200

-- | Created 201
status201 :: Status
status201 = Status 201 "Created"

-- | Created 201
created201 :: Status
created201 = status201

-- | Accepted 202
status202 :: Status
status202 = Status 202 "Accepted"

-- | Accepted 202
accepted202 :: Status
accepted202 = status202

-- | Non-Authoritative Information 203
status203 :: Status
status203 = Status 203 "Non-Authoritative Information"

-- | Non-Authoritative Information 203
nonAuthoritative203 :: Status
nonAuthoritative203 = status203

-- | No Content 204
status204 :: Status
status204 = Status 204 "No Content"

-- | No Content 204
noContent204 :: Status
noContent204 = status204

-- | Reset Content 205
status205 :: Status
status205 = Status 205 "Reset Content"

-- | Reset Content 205
resetContent205 :: Status
resetContent205 = status205

-- | Partial Content 206
status206 :: Status
status206 = Status 206 "Partial Content"

-- | Partial Content 206
partialContent206 :: Status
partialContent206 = status206

-- | Multiple Choices 300
status300 :: Status
status300 = Status 300 "Multiple Choices"

-- | Multiple Choices 300
multipleChoices300 :: Status
multipleChoices300 = status300

-- | Moved Permanently 301
status301 :: Status
status301 = Status 301 "Moved Permanently"

-- | Moved Permanently 301
movedPermanently301 :: Status
movedPermanently301 = status301

-- | Found 302
status302 :: Status
status302 = Status 302 "Found"

-- | Found 302
found302 :: Status
found302 = status302

-- | See Other 303
status303 :: Status
status303 = Status 303 "See Other"

-- | See Other 303
seeOther303 :: Status
seeOther303 = status303

-- | Not Modified 304
status304 :: Status
status304 = Status 304 "Not Modified"

-- | Not Modified 304
notModified304 :: Status
notModified304 = status304

-- | Use Proxy 305
status305 :: Status
status305 = Status 305 "Use Proxy"

-- | Use Proxy 305
useProxy305 :: Status
useProxy305 = status305

-- | Temporary Redirect 307
status307 :: Status
status307 = Status 307 "Temporary Redirect"

-- | Temporary Redirect 307
temporaryRedirect307 :: Status
temporaryRedirect307 = status307

-- | Bad Request 400
status400 :: Status
status400 = Status 400 "Bad Request"

-- | Bad Request 400
badRequest400 :: Status
badRequest400 = status400

-- | Unauthorized 401
status401 :: Status
status401 = Status 401 "Unauthorized"

-- | Unauthorized 401
unauthorized401 :: Status
unauthorized401 = status401

-- | Payment Required 402
status402 :: Status
status402 = Status 402 "Payment Required"

-- | Payment Required 402
paymentRequired402 :: Status
paymentRequired402 = status402

-- | Forbidden 403
status403 :: Status
status403 = Status 403 "Forbidden"

-- | Forbidden 403
forbidden403 :: Status
forbidden403 = status403

-- | Not Found 404
status404 :: Status
status404 = Status 404 "Not Found"

-- | Not Found 404
notFound404 :: Status
notFound404 = status404

-- | Method Not Allowed 405
status405 :: Status
status405 = Status 405 "Method Not Allowed"

-- | Method Not Allowed 405
methodNotAllowed405 :: Status
methodNotAllowed405 = status405

-- | Not Acceptable 406
status406 :: Status
status406 = Status 406 "Not Acceptable"

-- | Not Acceptable 406
notAcceptable406 :: Status
notAcceptable406 = status406

-- | Proxy Authentication Required 407
status407 :: Status
status407 = Status 407 "Proxy Authentication Required"

-- | Proxy Authentication Required 407
proxyAuthenticationRequired407 :: Status
proxyAuthenticationRequired407 = status407

-- | Request Timeout 408
status408 :: Status
status408 = Status 408 "Request Timeout"

-- | Request Timeout 408
requestTimeout408 :: Status
requestTimeout408 = status408

-- | Conflict 409
status409 :: Status
status409 = Status 409 "Conflict"

-- | Conflict 409
conflict409 :: Status
conflict409 = status409

-- | Gone 410
status410 :: Status
status410 = Status 410 "Gone"

-- | Gone 410
gone410 :: Status
gone410 = status410

-- | Length Required 411
status411 :: Status
status411 = Status 411 "Length Required"

-- | Length Required 411
lengthRequired411 :: Status
lengthRequired411 = status411

-- | Precondition Failed 412
status412 :: Status
status412 = Status 412 "Precondition Failed"

-- | Precondition Failed 412
preconditionFailed412 :: Status
preconditionFailed412 = status412

-- | Request Entity Too Large 413
status413 :: Status
status413 = Status 413 "Request Entity Too Large"

-- | Request Entity Too Large 413
requestEntityTooLarge413 :: Status
requestEntityTooLarge413 = status413

-- | Request-URI Too Long 414
status414 :: Status
status414 = Status 414 "Request-URI Too Long"

-- | Request-URI Too Long 414
requestURITooLong414 :: Status
requestURITooLong414 = status414

-- | Unsupported Media Type 415
status415 :: Status
status415 = Status 415 "Unsupported Media Type"

-- | Unsupported Media Type 415
unsupportedMediaType415 :: Status
unsupportedMediaType415 = status415

-- | Requested Range Not Satisfiable 416
status416 :: Status
status416 = Status 416 "Requested Range Not Satisfiable"

-- | Requested Range Not Satisfiable 416
requestedRangeNotSatisfiable416 :: Status
requestedRangeNotSatisfiable416 = status416

-- | Expectation Failed 417
status417 :: Status
status417 = Status 417 "Expectation Failed"

-- | Expectation Failed 417
expectationFailed417 :: Status
expectationFailed417 = status417

-- | I'm a teapot 418
status418 :: Status
status418 = Status 418 "I'm a teapot"

-- | I'm a teapot 418
imATeaPot418 :: Status
imATeaPot418 = status418

-- | Internal Server Error 500
status500 :: Status
status500 = Status 500 "Internal Server Error"

-- | Internal Server Error 500
internalServerError500 :: Status
internalServerError500 = status500

-- | Not Implemented 501
status501 :: Status
status501 = Status 501 "Not Implemented"

-- | Not Implemented 501
notImplemented501 :: Status
notImplemented501 = status501

-- | Bad Gateway 502
status502 :: Status
status502 = Status 502 "Bad Gateway"

-- | Bad Gateway 502
badGateway502 :: Status
badGateway502 = status502

-- | Service Unavailable 503
status503 :: Status
status503 = Status 503 "Service Unavailable"

-- | Service Unavailable 503
serviceUnavailable503 :: Status
serviceUnavailable503 = status503

-- | Gateway Timeout 504
status504 :: Status
status504 = Status 504 "Gateway Timeout"

-- | Gateway Timeout 504
gatewayTimeout504 :: Status
gatewayTimeout504 = status504

-- | HTTP Version Not Supported 505
status505 :: Status
status505 = Status 505 "HTTP Version Not Supported"

-- | HTTP Version Not Supported 505
httpVersionNotSupported505 :: Status
httpVersionNotSupported505 = status505

-- | Header
type Header = (CI.CI Ascii, Ascii)

-- | Request Headers
type RequestHeaders = [Header]

-- | Response Headers
type ResponseHeaders = [Header]

-- | HTTP Headers
headerAccept, headerAuthorization, headerCacheControl, headerConnection, headerContentLength, headerContentType, headerContentMD5, headerDate :: Ascii -> Header
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

renderByteRange :: ByteRange -> Ascii
renderByteRange = Blaze.toByteString . renderByteRangeBuilder

-- | RFC 2616 Byte ranges (set).
type ByteRanges = [ByteRange]

renderByteRangesBuilder :: ByteRanges -> Blaze.Builder
renderByteRangesBuilder xs = Blaze.copyByteString "bytes=" `mappend` 
                             mconcat (intersperse (Blaze.fromChar ',') (map renderByteRangeBuilder xs))

renderByteRanges :: ByteRanges -> Ascii
renderByteRanges = Blaze.toByteString . renderByteRangesBuilder

-- | Query item
type QueryItem = (B.ByteString, Maybe B.ByteString)

-- | Query.
-- 
-- General form: a=b&c=d, but if the value is Nothing, it becomes
-- a&c=d.
type Query = [QueryItem]

type QueryText = [(Text, Maybe Text)]

queryTextToQuery :: QueryText -> Query
queryTextToQuery = map $ encodeUtf8 *** fmap encodeUtf8

renderQueryText :: Bool -- ^ prepend a question mark?
                -> QueryText
                -> Blaze.Builder
renderQueryText b = renderQueryBuilder b . queryTextToQuery

queryToQueryText :: Query -> QueryText
queryToQueryText =
    map $ go *** fmap go
  where
    go = decodeUtf8With lenientDecode

parseQueryText :: B.ByteString -> QueryText
parseQueryText = queryToQueryText . parseQuery

-- | Simplified Query item type without support for parameter-less items.
type SimpleQueryItem = (B.ByteString, B.ByteString)

-- | Simplified Query type without support for parameter-less items.
type SimpleQuery = [SimpleQueryItem]

-- | Convert 'SimpleQuery' to 'Query'.
simpleQueryToQuery :: SimpleQuery -> Query
simpleQueryToQuery = map (\(a, b) -> (a, Just b))

renderQueryBuilder :: Bool -- ^ prepend a question mark?
                   -> Query
                   -> Blaze.Builder
renderQueryBuilder _ [] = mempty
-- FIXME replace mconcat + map with foldr
renderQueryBuilder qmark' (p:ps) = mconcat
    $ go (if qmark' then qmark else mempty) p
    : map (go amp) ps
  where
    qmark = Blaze.copyByteString "?"
    amp = Blaze.copyByteString "&"
    equal = Blaze.copyByteString "="
    go sep (k, mv) = mconcat [
                      sep
                     , urlEncodeBuilder True k
                     , case mv of
                         Nothing -> mempty
                         Just v -> equal `mappend` urlEncodeBuilder True v
                     ]

-- | Convert 'Query' to 'ByteString'.
renderQuery :: Bool -- ^ prepend question mark?
            -> Query -> Ascii
renderQuery qm = Blaze.toByteString . renderQueryBuilder qm

-- | Convert 'SimpleQuery' to 'ByteString'.
renderSimpleQuery :: Bool -- ^ prepend question mark?
                  -> SimpleQuery -> Ascii
renderSimpleQuery useQuestionMark = renderQuery useQuestionMark . simpleQueryToQuery

-- | Split out the query string into a list of keys and values. A few
-- importants points:
--
-- * The result returned is still bytestrings, since we perform no character
-- decoding here. Most likely, you will want to use UTF-8 decoding, but this is
-- left to the user of the library.
--
-- * Percent decoding errors are ignored. In particular, "%Q" will be output as
-- "%Q".
parseQuery :: B.ByteString -> Query
parseQuery = parseQueryString' . dropQuestion
  where
    dropQuestion q =
        case B.uncons q of
            Just (63, q') -> q'
            _ -> q
    parseQueryString' q | B.null q = []
    parseQueryString' q =
        let (x, xs) = breakDiscard queryStringSeparators q
         in parsePair x : parseQueryString' xs
      where
        parsePair x =
            let (k, v) = B.breakByte 61 x -- equal sign
                v'' =
                    case B.uncons v of
                        Just (_, v') -> Just $ urlDecode True v'
                        _ -> Nothing
             in (urlDecode True k, v'')

queryStringSeparators :: B.ByteString
queryStringSeparators = B.pack [38,59] -- ampersand, semicolon

-- | Break the second bytestring at the first occurence of any bytes from
-- the first bytestring, discarding that byte.
breakDiscard :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
breakDiscard seps s =
    let (x, y) = B.break (`B.elem` seps) s
     in (x, B.drop 1 y)

-- | Parse 'SimpleQuery' from a 'ByteString'.
parseSimpleQuery :: B.ByteString -> SimpleQuery
parseSimpleQuery = map (second $ fromMaybe B.empty) . parseQuery

ord8 :: Char -> Word8
ord8 = fromIntegral . ord

unreservedQS, unreservedPI :: [Word8]
unreservedQS = map ord8 "-_.~"
unreservedPI = map ord8 "-_.~:@&=+$,"

-- | Percent-encoding for URLs.
urlEncodeBuilder' :: [Word8] -> B.ByteString -> Blaze.Builder
urlEncodeBuilder' extraUnreserved = mconcat . map encodeChar . B.unpack
    where
      encodeChar ch | unreserved ch = Blaze.fromWord8 ch
                    | otherwise     = h2 ch
      
      unreserved ch | ch >= 65 && ch <= 90  = True -- A-Z
                    | ch >= 97 && ch <= 122 = True -- a-z
                    | ch >= 48 && ch <= 57  = True -- 0-9
      unreserved c = c `elem` extraUnreserved
      
      h2 v = let (a, b) = v `divMod` 16 in Blaze.fromWord8s [37, h a, h b] -- percent (%)
      h i | i < 10    = 48 + i -- zero (0)
          | otherwise = 65 + i - 10 -- 65: A

urlEncodeBuilder
    :: Bool -- ^ Whether input is in query string. True: Query string, False: Path element
    -> B.ByteString
    -> Blaze.Builder
urlEncodeBuilder True  = urlEncodeBuilder' unreservedQS
urlEncodeBuilder False = urlEncodeBuilder' unreservedPI

urlEncode :: Bool -> B.ByteString -> Ascii
urlEncode q = Blaze.toByteString . urlEncodeBuilder q

-- | Percent-decoding.
urlDecode :: Bool -- ^ Whether to decode '+' to ' '
          -> B.ByteString -> B.ByteString
urlDecode replacePlus z = fst $ B.unfoldrN (B.length z) go z
  where
    go bs =
        case B.uncons bs of
            Nothing -> Nothing
            Just (43, ws) | replacePlus -> Just (32, ws) -- plus to space
            Just (37, ws) -> Just $ fromMaybe (37, ws) $ do -- percent
                (x, xs) <- B.uncons ws
                x' <- hexVal x
                (y, ys) <- B.uncons xs
                y' <- hexVal y
                Just $ (combine x' y', ys)
            Just (w, ws) -> Just (w, ws)
    hexVal w
        | 48 <= w && w <= 57  = Just $ w - 48 -- 0 - 9
        | 65 <= w && w <= 70  = Just $ w - 55 -- A - F
        | 97 <= w && w <= 102 = Just $ w - 87 -- a - f
        | otherwise = Nothing
    combine :: Word8 -> Word8 -> Word8
    combine a b = shiftL a 4 .|. b

-- | Encodes a list of path segments into a valid URL fragment.
--
-- This function takes the following three steps:
--
-- * UTF-8 encodes the characters.
--
-- * Performs percent encoding on all unreserved characters, as well as \:\@\=\+\$,
--
-- * Prepends each segment with a slash.
--
-- For example:
--
-- > encodePathSegments [\"foo\", \"bar\", \"baz\"]
--
-- \"\/foo\/bar\/baz\"
--
-- > encodePathSegments [\"foo bar\", \"baz\/bin\"]
--
-- \"\/foo\%20bar\/baz\%2Fbin\"
--
-- > encodePathSegments [\"שלום\"]
--
-- \"\/%D7%A9%D7%9C%D7%95%D7%9D\"
--
-- Huge thanks to Jeremy Shaw who created the original implementation of this
-- function in web-routes and did such thorough research to determine all
-- correct escaping procedures.
encodePathSegments :: [Text] -> Blaze.Builder
encodePathSegments [] = mempty
encodePathSegments (x:xs) =
    Blaze.copyByteString "/"
    `mappend` encodePathSegment x
    `mappend` encodePathSegments xs

-- | Like encodePathSegments, but without the initial slash.
encodePathSegmentsRelative :: [Text] -> Blaze.Builder
encodePathSegmentsRelative xs = mconcat $ intersperse (Blaze.copyByteString "/") (map encodePathSegment xs)

encodePathSegment :: Text -> Blaze.Builder
encodePathSegment = urlEncodeBuilder False . encodeUtf8

decodePathSegments :: B.ByteString -> [Text]
decodePathSegments "" = []
decodePathSegments "/" = []
decodePathSegments a =
    go $ drop1Slash a
  where
    drop1Slash bs =
        case B.uncons bs of
            Just (47, bs') -> bs' -- 47 == /
            _ -> bs
    go bs =
        let (x, y) = B.breakByte 47 bs
         in decodePathSegment x :
            if B.null y
                then []
                else go $ B.drop 1 y

decodePathSegment :: B.ByteString -> Text
decodePathSegment = decodeUtf8With lenientDecode . urlDecode False

encodePath :: [Text] -> Query -> Blaze.Builder
encodePath x [] = encodePathSegments x
encodePath x y = encodePathSegments x `mappend` renderQueryBuilder True y

decodePath :: B.ByteString -> ([Text], Query)
decodePath b =
    let (x, y) = B.breakByte 63 b -- question mark
    in (decodePathSegments x, parseQuery y)
