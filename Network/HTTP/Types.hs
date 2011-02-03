module Network.HTTP.Types
(
  -- * Case insensitive HTTP ByteStrings
  HttpCIByteString(..)
, mkHttpCIByteString
  -- * Methods
, Method
, MethodADT(GET, POST, HEAD, PUT, DELETE, TRACE, CONNECT, OPTIONS)
, methodToADT
, methodFromADT
, stringToMethodADT
, methodADTToString
  -- * Versions
, HttpVersion(httpMajor, httpMinor)
, http09
, http10
, http11
  -- * Status
, Status(statusCode, statusMessage)
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
, RequestHeaders
, ResponseHeaders
  -- * Query string
, Query
, QuerySimple
)
where

import           Data.Char
import           Data.Maybe
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as Ascii

localError :: String -> String -> a
localError f s = error $ "Network.HTTP.Types." ++ f ++ ": " ++ s

-- | Case-insensitive HTTP ByteStrings, mostly for use in Header names.
data HttpCIByteString
    = HttpCIByteString {
        ciOriginal :: !B.ByteString
      , ciLowerCase :: !B.ByteString
      }

mkHttpCIByteString :: B.ByteString -> HttpCIByteString
mkHttpCIByteString orig = HttpCIByteString {
                            ciOriginal = orig
                          , ciLowerCase = Ascii.map toLower orig
                          }

instance Eq HttpCIByteString where
    HttpCIByteString { ciLowerCase = a } == HttpCIByteString { ciLowerCase = b } 
        = a == b

instance Ord HttpCIByteString where
    compare HttpCIByteString { ciLowerCase = a } HttpCIByteString { ciLowerCase = b } 
        = compare a b

instance Show HttpCIByteString where
    show = show . ciOriginal

-- | HTTP method (flat string type).
type Method = B.ByteString

-- | HTTP method (ADT version).
-- 
-- Note that the Show instance is only for debugging and should NOT be used to generate HTTP method strings; use 'methodToByteString' instead.
-- 
-- The constructor 'OtherMethod' is not exported for forwards compatibility reasons.
data MethodADT
    = GET
    | POST
    | HEAD  
    | PUT
    | DELETE
    | TRACE
    | CONNECT
    | OPTIONS
    | OtherMethod B.ByteString
    deriving (Show, Eq, Ord)

-- These are ordered by suspected frequency. More popular methods should go first.
-- The reason is that methodListA and methodListB are used with lookup.
-- lookup is probably faster for these few cases than setting up an elaborate data structure.
methodListA :: [(B.ByteString, MethodADT)]
methodListA 
    = [ (Ascii.pack "GET", GET)
      , (Ascii.pack "POST", POST)
      , (Ascii.pack "HEAD", HEAD)
      , (Ascii.pack "PUT", PUT)
      , (Ascii.pack "DELETE", DELETE)
      , (Ascii.pack "TRACE", TRACE)
      , (Ascii.pack "CONNECT", CONNECT)
      , (Ascii.pack "OPTIONS", OPTIONS)
      ]

methodListB :: [(MethodADT, B.ByteString)]
methodListB = map (\(a, b) -> (b, a)) methodListA

-- | Convert a method 'ByteString' to a 'MethodADT'.
methodToADT :: Method -> MethodADT
methodToADT bs = fromMaybe (OtherMethod bs) $ lookup bs methodListA

-- | Convert a 'MethodADT' to a 'ByteString'.
methodFromADT :: MethodADT -> Method
methodFromADT m
    = case m of
        OtherMethod bs -> bs
        _ -> fromMaybe (localError "methodToByteString" "This should not happen (methodListB is incomplete)") $
             lookup m methodListB

-- | Convert a method 'String' to a 'MethodADT'.
stringToMethodADT :: String -> MethodADT
stringToMethodADT = methodToADT . Ascii.pack

-- | Convert a 'MethodADT' to a 'String'.
methodADTToString :: MethodADT -> String
methodADTToString = Ascii.unpack . methodFromADT

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

-- | Request Header
type RequestHeaders = [(HttpCIByteString, B.ByteString)]

-- | Response Headers
type ResponseHeaders = [(HttpCIByteString, B.ByteString)]

-- | Query.
-- 
-- General form: a=b&c=d, but if the value is Nothing, it becomes
-- a&c=d.
type Query = [(B.ByteString, Maybe B.ByteString)]

-- | Simplified Query type without support for parameter-less items.
type QuerySimple = [(B.ByteString, B.ByteString)]
