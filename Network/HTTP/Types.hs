module Network.HTTP.Types
(
  -- * Case insensitive HTTP ByteStrings
  CIByteString(..)
, mkCIByteString
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
, renderQuery
, renderSimpleQuery
, parseQuery
, parseSimpleQuery
  -- * URL encoding / decoding
, urlEncode
, urlDecode
)
where

import           Control.Arrow         (second, (|||))
import           Data.Array
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.String
import           Numeric
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as Ascii

-- | Case-insensitive HTTP ByteStrings, mostly for use in Header names.
data CIByteString
    = CIByteString {
        ciOriginal :: !B.ByteString
      , ciLowerCase :: !B.ByteString
      }

mkCIByteString :: B.ByteString -> CIByteString
mkCIByteString orig = CIByteString {
                            ciOriginal = orig
                          , ciLowerCase = Ascii.map toLower orig
                          }

instance Eq CIByteString where
    CIByteString { ciLowerCase = a } == CIByteString { ciLowerCase = b } 
        = a == b

instance Ord CIByteString where
    compare CIByteString { ciLowerCase = a } CIByteString { ciLowerCase = b } 
        = compare a b

instance Show CIByteString where
    show = show . ciOriginal

instance IsString CIByteString where
    fromString = mkCIByteString . Ascii.pack

-- | HTTP method (flat string type).
type Method = B.ByteString

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
-- 
-- Note that the Show instance is only for debugging and should NOT be used to generate HTTP method strings; use 'methodToByteString' instead.
-- 
-- The constructor 'OtherMethod' is not exported for forwards compatibility reasons.
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
methodArray = listArray (minBound, maxBound) $ map (Ascii.pack . show) [minBound :: StdMethod .. maxBound]

methodList :: [(Method, StdMethod)]
methodList = map (\(a, b) -> (b, a)) (assocs methodArray)

-- | Convert a method 'ByteString' to a 'StdMethod' if possible.
parseMethod :: Method -> Either B.ByteString StdMethod
parseMethod bs = maybe (Left bs) Right $ lookup bs methodList

-- | Convert an algebraic method to a 'ByteString'.
renderMethod :: Either B.ByteString StdMethod -> Method
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
      , statusMessage :: B.ByteString
      }
    deriving (Show)

instance Eq Status where
    Status { statusCode = a } == Status { statusCode = b } = a == b

instance Ord Status where
    compare Status { statusCode = a } Status { statusCode = b } = a `compare` b

-- | OK
status200, statusOK :: Status
status200 = Status 200 $ Ascii.pack "OK"
statusOK = status200

-- | Created
status201, statusCreated :: Status
status201 = Status 200 $ Ascii.pack "Created"
statusCreated = status201

-- | Moved Permanently
status301, statusMovedPermanently :: Status
status301 = Status 301 $ Ascii.pack "Moved Permanently"
statusMovedPermanently = status301

-- | Found
status302, statusFound :: Status
status302 = Status 302 $ Ascii.pack "Found"
statusFound = status302

-- | See Other
status303, statusSeeOther :: Status
status303 = Status 303 $ Ascii.pack "See Other"
statusSeeOther = status303

-- | Bad Request
status400, statusBadRequest :: Status
status400 = Status 400 $ Ascii.pack "Bad Request"
statusBadRequest = status400

-- | Unauthorized
status401, statusUnauthorized :: Status
status401 = Status 401 $ Ascii.pack "Unauthorized"
statusUnauthorized = status401

-- | Forbidden
status403, statusForbidden :: Status
status403 = Status 403 $ Ascii.pack "Forbidden"
statusForbidden = status403

-- | Not Found
status404, statusNotFound :: Status
status404 = Status 404 $ Ascii.pack "Not Found"
statusNotFound = status404

-- | Method Not Allowed
status405, statusNotAllowed :: Status
status405 = Status 405 $ Ascii.pack "Method Not Allowed"
statusNotAllowed = status405

-- | Internal Server Error
status500, statusServerError :: Status
status500 = Status 500 $ Ascii.pack "Internal Server Error"
statusServerError = status500

-- | Header
type Header = (CIByteString, B.ByteString)

-- | Request Headers
type RequestHeaders = [Header]

-- | Response Headers
type ResponseHeaders = [Header]

makeHeader :: String -> B.ByteString -> Header
makeHeader a = \b -> (a', b)
    where a' = mkCIByteString $ Ascii.pack a

-- | HTTP Headers
headerAccept, headerCacheControl, headerConnection, headerContentLength, headerContentType, headerContentMD5, headerDate :: B.ByteString -> Header
headerAccept        = makeHeader "Accept"
headerCacheControl  = makeHeader "Cache-Control"
headerConnection    = makeHeader "Connection"
headerContentLength = makeHeader "Content-Length"
headerContentType   = makeHeader "Content-Type"
headerContentMD5    = makeHeader "Content-MD5"
headerDate          = makeHeader "Date"

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

-- | Convert 'Query' to 'ByteString'.
renderQuery :: Bool -> Query -> B.ByteString
renderQuery useQuestionMark = B.concat 
                              . addQuestionMark
                              . intercalate [Ascii.pack "&"] 
                              . map showQueryItem 
    where
      addQuestionMark :: [B.ByteString] -> [B.ByteString]
      addQuestionMark [] = []
      addQuestionMark xs | useQuestionMark = Ascii.pack "?" : xs
                         | otherwise       = xs
      
      showQueryItem :: (B.ByteString, Maybe B.ByteString) -> [B.ByteString]
      showQueryItem (n, Nothing) = [urlEncode n]
      showQueryItem (n, Just v) = [urlEncode n, Ascii.pack "=", urlEncode v]

-- | Convert 'SimpleQuery' to 'ByteString'.
renderSimpleQuery :: Bool -> SimpleQuery -> B.ByteString
renderSimpleQuery useQuestionMark = renderQuery useQuestionMark . map (\(k, v) -> (k, Just v))

-- | Parse 'Query' from a 'ByteString'.
parseQuery :: B.ByteString -> Query
parseQuery bs = case Ascii.uncons bs of
                  Nothing         -> []
                  Just ('?', bs') -> parseQuery' bs'
                  _               -> parseQuery' bs
    where
      parseQuery' = map parseQueryItem . Ascii.split '&'
      parseQueryItem q = (k, v)
        where (k', v') = Ascii.break (== '=') q
              k = urlDecode k'
              v = if B.null v'
                  then Nothing
                  else Just $ urlDecode $ B.tail v'

-- | Parse 'SimpleQuery' from a 'ByteString'.
parseSimpleQuery :: B.ByteString -> SimpleQuery
parseSimpleQuery = map (second $ fromMaybe B.empty) . parseQuery

-- | Percent-encoding for URLs.
urlEncode :: B.ByteString -> B.ByteString
urlEncode = Ascii.concatMap (Ascii.pack . encodeChar)
    where
      encodeChar :: Char -> [Char]
      encodeChar ch | unreserved ch = [ch]
                    | otherwise     = h2 $ ord ch
      
      unreserved :: Char -> Bool
      unreserved ch | ch >= 'A' && ch <= 'Z' = True 
                    | ch >= 'a' && ch <= 'z' = True
                    | ch >= '0' && ch <= '9' = True 
      unreserved '-' = True
      unreserved '_' = True
      unreserved '.' = True
      unreserved '~' = True
      unreserved _   = False
      
      h2 :: Int -> [Char]
      h2 v = let (a, b) = v `divMod` 16 in ['%', h a, h b]
      
      h :: Int -> Char
      h i | i < 10    = chr $ ord '0' + i
          | otherwise = chr $ ord 'A' + i - 10

-- | Percent-decoding.
urlDecode :: B.ByteString -> B.ByteString
urlDecode bs = case Ascii.uncons bs of
                 Nothing -> B.empty
                 Just ('%', x) -> case readHex $ Ascii.unpack pc of
                                    [(v, "")] -> chr v `Ascii.cons` urlDecode bs'
                                    _ -> Ascii.cons '%' $ urlDecode x
                     where (pc, bs') = Ascii.splitAt 2 x
                 Just (c, bs') -> Ascii.cons c $ urlDecode bs'
