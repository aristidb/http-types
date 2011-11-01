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
, status100, statusContinue
, status101, statusSwitchingProtocols
, status200, statusOK
, status201, statusCreated
, status202, statusAccepted
, status203, statusNonAuthoritative
, status204, statusNoContent
, status205, statusResetContent
, status206, statusPartialContent
, status300, statusMultipleChoices
, status301, statusMovedPermanently
, status302, statusFound
, status303, statusSeeOther
, status304, statusNotModified
, status305, statusUseProxy
, status307, statusTemporaryRedirect
, status400, statusBadRequest
, status401, statusUnauthorized
, status402, statusPaymentRequired
, status403, statusForbidden
, status404, statusNotFound
, status405, statusNotAllowed
, status406, statusNotAcceptable
, status407, statusProxyAuthenticationRequired
, status408, statusRequestTimeout
, status409, statusConflict
, status410, statusGone
, status411, statusLengthRequired
, status412, statusPreconditionFailed
, status413, statusRequestEntityTooLarge
, status414, statusRequestURITooLong
, status415, statusUnsupportedMediaType
, status416, statusRequestedRangeNotSatisfiable
, status417, statusExpectationFailed
, status418, statusImATeapot
, status500, statusServerError
, status501, statusNotImplemented
, status502, statusBadGateway
, status503, statusServiceUnavailable
, status504, statusGatewayTimeout
, status505, statusHTTPVersionNotSupported
  -- * Headers
, Header
, RequestHeaders
, ResponseHeaders
, headerAccept
, headerAuthorization
, headerCacheControl
, headerConnection
, headerContentLength
, headerContentType
, headerContentMD5
, headerDate
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
  -- * Path (segments + query string)
, encodePath
, decodePath
  -- * URL encoding / decoding
, urlEncodeBuilder
, urlEncode
, urlDecode
)
where

import           Control.Arrow            (second, (|||), (***))
import           Data.Array
import           Data.Bits                (shiftL, (.|.))
import           Data.Char
import           Data.Maybe
import           Data.Monoid              (mempty, mappend, mconcat)
import           Data.Text                (Text)
import           Data.Text.Encoding       (encodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Word                (Word8)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as B8
import qualified Data.CaseInsensitive     as CI

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

-- | Continue
status100, statusContinue :: Status
status100 = Status 100 "Continue"
statusContinue = status100

-- | Switching Protocols
status101, statusSwitchingProtocols :: Status
status101 = Status 101 "Switching Protocols"
statusSwitchingProtocols = status101

-- | OK
status200, statusOK :: Status
status200 = Status 200 "OK"
statusOK = status200

-- | Created
status201, statusCreated :: Status
status201 = Status 201 "Created"
statusCreated = status201

-- | Accepted
status202, statusAccepted :: Status
status202 = Status 202 "Accepted"
statusAccepted = status202

-- | Non-Authoritative Information
status203, statusNonAuthoritative :: Status
status203 = Status 203 "Non-Authoritative Information"
statusNonAuthoritative = status203

-- | No Content
status204, statusNoContent :: Status
status204 = Status 204 "No Content"
statusNoContent = status204

-- | Reset Content
status205, statusResetContent :: Status
status205 = Status 205 "Reset Content"
statusResetContent = status205

-- | PartialContent
status206, statusPartialContent :: Status
status206 = Status 206 "PartialContent"
statusPartialContent = status206

-- | Multiple Choices
status300, statusMultipleChoices :: Status
status300 = Status 300 "Multiple Choices"
statusMultipleChoices = status300

-- | Moved Permanently
status301, statusMovedPermanently :: Status
status301 = Status 301 "Moved Permanently"
statusMovedPermanently = status301

-- | Found
status302, statusFound :: Status
status302 = Status 302 "Found"
statusFound = status302

-- | See Other
status303, statusSeeOther :: Status
status303 = Status 303 "See Other"
statusSeeOther = status303

-- | Not Modified
status304, statusNotModified :: Status
status304 = Status 304 "Not Modified"
statusNotModified = status304

-- | Use Proxy
status305, statusUseProxy :: Status
status305 = Status 305 "Use Proxy"
statusUseProxy = status305

-- | Temporary Redirect
status307, statusTemporaryRedirect :: Status
status307 = Status 307 "Temporary Redirect"
statusTemporaryRedirect = status307

-- | Bad Request
status400, statusBadRequest :: Status
status400 = Status 400 "Bad Request"
statusBadRequest = status400

-- | Unauthorized
status401, statusUnauthorized :: Status
status401 = Status 401 "Unauthorized"
statusUnauthorized = status401

-- | Payment Required
status402, statusPaymentRequired :: Status
status402 = Status 402 "Payment Required"
statusPaymentRequired = status402

-- | Forbidden
status403, statusForbidden :: Status
status403 = Status 403 "Forbidden"
statusForbidden = status403

-- | Not Found
status404, statusNotFound :: Status
status404 = Status 404 "Not Found"
statusNotFound = status404

-- | Method Not Allowed
status405, statusNotAllowed :: Status
status405 = Status 405 "Method Not Allowed"
statusNotAllowed = status405

-- | Not Acceptable
status406, statusNotAcceptable :: Status
status406 = Status 406 "Not Acceptable"
statusNotAcceptable = status406

-- | Proxy Authentication Required
status407, statusProxyAuthenticationRequired :: Status
status407 = Status 407 "Proxy Authentication Required"
statusProxyAuthenticationRequired = status407

-- | Request Timeout
status408, statusRequestTimeout :: Status
status408 = Status 408 "Request Timeout"
statusRequestTimeout = status408

-- | Conflict
status409, statusConflict :: Status
status409 = Status 409 "Conflict"
statusConflict = status409

-- | Gone
status410, statusGone :: Status
status410 = Status 410 "Gone"
statusGone = status410

-- | Length Required
status411, statusLengthRequired :: Status
status411 = Status 411 "Length Required"
statusLengthRequired = status411

-- | Precondition Failed
status412, statusPreconditionFailed :: Status
status412 = Status 412 "Precondition Failed"
statusPreconditionFailed = status412

-- | Request Entity Too Large
status413, statusRequestEntityTooLarge :: Status
status413 = Status 413 "Request Entity Too Large"
statusRequestEntityTooLarge = status413

-- | Request-URI Too Long
status414, statusRequestURITooLong :: Status
status414 = Status 414 "Request-URI Too Long"
statusRequestURITooLong = status414


-- | Unsupported Media Type
status415, statusUnsupportedMediaType :: Status
status415 = Status 415 "Unsupported Media Type"
statusUnsupportedMediaType = status415

-- | Requested Range Not Satisfiable
status416, statusRequestedRangeNotSatisfiable :: Status
status416 = Status 416 "Requested Range Not Satisfiable"
statusRequestedRangeNotSatisfiable = status416

-- | Expectation Failed
status417, statusExpectationFailed :: Status
status417 = Status 417 "Expectation Failed"
statusExpectationFailed = status417

-- | I'm a teapot
status418, statusImATeapot :: Status
status418 = Status 418 "I'm a teapot"
statusImATeapot = status418

-- | Internal Server Error
status500, statusServerError :: Status
status500 = Status 500 "Internal Server Error"
statusServerError = status500

-- | Not Implemented
status501, statusNotImplemented :: Status
status501 = Status 501 "Not Implemented"
statusNotImplemented = status501

-- | Bad Gateway
status502, statusBadGateway :: Status
status502 = Status 502 "Bad Gateway"
statusBadGateway = status502

-- | Service Unavailable
status503, statusServiceUnavailable :: Status
status503 = Status 503 "Service Unavailable"
statusServiceUnavailable = status503

-- | Gateway Timeout
status504, statusGatewayTimeout :: Status
status504 = Status 504 "Gateway Timeout"
statusGatewayTimeout = status504

-- | HTTP Version Not Supported
status505, statusHTTPVersionNotSupported :: Status
status505 = Status 505 "HTTP Version Not Supported"
statusHTTPVersionNotSupported = status505

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
-- * Intercalates with a slash.
--
-- For example:
--
-- > encodePathInfo [\"foo\", \"bar\", \"baz\"]
--
-- \"foo\/bar\/baz\"
--
-- > encodePathInfo [\"foo bar\", \"baz\/bin\"]
--
-- \"foo\%20bar\/baz\%2Fbin\"
--
-- > encodePathInfo [\"שלום\"]
--
-- \"%D7%A9%D7%9C%D7%95%D7%9D\"
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
    let (x, y) = B.breakByte 63 b -- slash
    in (decodePathSegments x, parseQuery y)
