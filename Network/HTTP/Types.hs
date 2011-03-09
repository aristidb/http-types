{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Types
(
  -- * Methods
  Method
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
, status200, statusOK
, status201, statusCreated
, status301, statusMovedPermanently
, status302, statusFound
, status303, statusSeeOther
, status400, statusBadRequest
, status401, statusUnauthorized
, status403, statusForbidden
, status404, statusNotFound
, status405, statusNotAllowed
, status500, statusServerError
  -- * Headers
, Header
, RequestHeaders
, ResponseHeaders
, headerAccept
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
  -- * Path segments
, encodePathSegments
, decodePathSegments
  -- * Path (segments + query string)
, encodePath
, decodePath
  -- * URL encoding / decoding
, urlEncode
, urlDecode
)
where

import           Control.Arrow         (second, (|||))
import           Data.Array
import           Data.Char
import           Data.Maybe
import           Numeric
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as Ascii
import qualified Data.Ascii               as A
import           Data.Word                   (Word8)
import           Data.Bits                   (shiftL, (.|.))
import qualified Blaze.ByteString.Builder as Blaze
import           Data.Monoid                 (mempty, mappend, mconcat)
import           Data.Text                   (Text)
import           Data.Text.Encoding          (encodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error    (lenientDecode)

-- | HTTP method (flat string type).
type Method = A.Ascii

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
methodArray = listArray (minBound, maxBound) $ map (A.unsafeFromString . show) [minBound :: StdMethod .. maxBound]

methodList :: [(Method, StdMethod)]
methodList = map (\(a, b) -> (b, a)) (assocs methodArray)

-- | Convert a method 'ByteString' to a 'StdMethod' if possible.
parseMethod :: Method -> Either A.Ascii StdMethod
parseMethod bs = maybe (Left bs) Right $ lookup bs methodList

-- | Convert an algebraic method to a 'ByteString'.
renderMethod :: Either A.Ascii StdMethod -> Method
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
      , statusMessage :: A.Ascii
      }
    deriving (Show)

instance Eq Status where
    Status { statusCode = a } == Status { statusCode = b } = a == b

instance Ord Status where
    compare Status { statusCode = a } Status { statusCode = b } = a `compare` b

-- | OK
status200, statusOK :: Status
status200 = Status 200 "OK"
statusOK = status200

-- | Created
status201, statusCreated :: Status
status201 = Status 201 "Created"
statusCreated = status201

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

-- | Bad Request
status400, statusBadRequest :: Status
status400 = Status 400 "Bad Request"
statusBadRequest = status400

-- | Unauthorized
status401, statusUnauthorized :: Status
status401 = Status 401 "Unauthorized"
statusUnauthorized = status401

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

-- | Internal Server Error
status500, statusServerError :: Status
status500 = Status 500 "Internal Server Error"
statusServerError = status500

-- | Header
type Header = (A.CIAscii, A.Ascii)

-- | Request Headers
type RequestHeaders = [Header]

-- | Response Headers
type ResponseHeaders = [Header]

-- | HTTP Headers
headerAccept, headerCacheControl, headerConnection, headerContentLength, headerContentType, headerContentMD5, headerDate :: A.Ascii -> Header
headerAccept        = (,) "Accept"
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

-- | Simplified Query item type without support for parameter-less items.
type SimpleQueryItem = (B.ByteString, B.ByteString)

-- | Simplified Query type without support for parameter-less items.
type SimpleQuery = [SimpleQueryItem]

-- | Convert 'SimpleQuery' to 'Query'.
simpleQueryToQuery :: SimpleQuery -> Query
simpleQueryToQuery = map (\(a, b) -> (a, Just b))

renderQueryBuilder :: Bool -- ^ prepend a question mark?
                   -> Query
                   -> A.AsciiBuilder
renderQueryBuilder False [] = mempty
renderQueryBuilder True [] = A.unsafeFromBuilder $ Blaze.copyByteString "?"
-- FIXME replace mconcat + map with foldr
renderQueryBuilder qmark' (p:ps) = mconcat
    $ go (if qmark' then qmark else mempty) p
    : map (go amp) ps
  where
    qmark = A.unsafeFromBuilder $ Blaze.copyByteString "?"
    amp = A.unsafeFromBuilder $ Blaze.copyByteString "&"
    equal = A.unsafeFromBuilder $ Blaze.copyByteString "="
    go sep (k, mv) =
        sep
        `mappend` A.unsafeFromBuilder (Blaze.copyByteString (urlEncode unreservedQS k))
        `mappend`
            (case mv of
                Nothing -> mempty
                Just v -> equal `mappend`
                          A.unsafeFromBuilder (Blaze.copyByteString (urlEncode unreservedQS v)))

-- | Convert 'Query' to 'ByteString'.
renderQuery :: Bool -- ^ prepend question mark?
            -> Query -> A.Ascii
renderQuery qm = A.fromAsciiBuilder . renderQueryBuilder qm

-- | Convert 'SimpleQuery' to 'ByteString'.
renderSimpleQuery :: Bool -- ^ prepend question mark?
                  -> SimpleQuery -> A.Ascii
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
        let (x, xs) = breakDiscard 38 q -- ampersand
         in parsePair x : parseQueryString' xs
      where
        parsePair x =
            let (k, v) = B.breakByte 61 x -- equal sign
                v'' =
                    case B.uncons v of
                        Just (_, v') -> Just $ urlDecode True v'
                        _ -> Nothing
             in (urlDecode True k, v'')

breakDiscard :: Word8 -> B.ByteString -> (B.ByteString, B.ByteString)
breakDiscard w s =
    let (x, y) = B.breakByte w s
     in (x, B.drop 1 y)

-- | Parse 'SimpleQuery' from a 'ByteString'.
parseSimpleQuery :: B.ByteString -> SimpleQuery
parseSimpleQuery = map (second $ fromMaybe B.empty) . parseQuery

unreservedQS, unreservedPI :: String
unreservedQS = "-_.~"
unreservedPI = ":@&=+$,"

-- | Percent-encoding for URLs. Note that this is only valid for query string
-- parameters, not other URL components.
urlEncode :: String -> B.ByteString -> B.ByteString -- FIXME more efficient, use Builder
urlEncode extraUnreserved = Ascii.concatMap (Ascii.pack . encodeChar)
    where
      encodeChar :: Char -> [Char]
      encodeChar ch | unreserved ch = [ch]
                    | otherwise     = h2 $ ord ch
      
      unreserved :: Char -> Bool
      unreserved ch | ch >= 'A' && ch <= 'Z' = True 
                    | ch >= 'a' && ch <= 'z' = True
                    | ch >= '0' && ch <= '9' = True 
      unreserved c = c `elem` extraUnreserved
      
      h2 :: Int -> [Char]
      h2 v = let (a, b) = v `divMod` 16 in ['%', h a, h b]
      
      h :: Int -> Char
      h i | i < 10    = chr $ ord '0' + i
          | otherwise = chr $ ord 'A' + i - 10

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
encodePathSegments :: [Text] -> A.AsciiBuilder
encodePathSegments [] = mempty
encodePathSegments (x:xs) =
    A.unsafeFromBuilder (Blaze.copyByteString "/")
    `mappend` encodePathSegment x
    `mappend` encodePathSegments xs

encodePathSegment :: Text -> A.AsciiBuilder
encodePathSegment = A.unsafeFromBuilder . Blaze.fromByteString . urlEncode unreservedPI . encodeUtf8

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

encodePath :: [Text] -> Query -> A.AsciiBuilder
encodePath x [] = encodePathSegments x
encodePath x y = encodePathSegments x `mappend` renderQueryBuilder True y

decodePath :: B.ByteString -> ([Text], Query)
decodePath b =
    let (x, y) = B.breakByte 63 b -- slash
    in (decodePathSegments x, parseQuery y)
