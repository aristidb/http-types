module Network.HTTP.Types
(
  -- * Methods
  Method(GET, POST, HEAD, PUT, DELETE, TRACE, CONNECT, OPTIONS)
, byteStringToMethod
, methodToByteString
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
)
where

import           Data.Char
import           Data.Maybe
import qualified Data.ByteString.Char8 as Ascii

localError :: String -> String -> a
localError f s = error $ "Network.HTTP.Types." ++ f ++ ": " ++ s

-- | HTTP method.
-- 
-- Note that the Show instance is only for debugging and should NOT be used to generate HTTP method strings; use 'methodToByteString' instead.
-- 
-- The constructor 'OtherMethod' is not exported for forwards compatibility reasons.
data Method
    = GET
    | POST
    | HEAD  
    | PUT
    | DELETE
    | TRACE
    | CONNECT
    | OPTIONS
    | OtherMethod Ascii.ByteString
    deriving (Show, Eq, Ord)

-- These are ordered by suspected frequency. More popular methods should go first.
-- The reason is that methodListA and methodListB are used with lookup.
-- lookup is probably faster for these few cases than setting up an elaborate data structure.
methodListA :: [(Ascii.ByteString, Method)]
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

methodListB :: [(Method, Ascii.ByteString)]
methodListB = map (\(a, b) -> (b, a)) methodListA

-- | Convert a method 'ByteString' to a 'Method'.
byteStringToMethod :: Ascii.ByteString -> Method
byteStringToMethod bs' = fromMaybe (OtherMethod bs) $ lookup bs methodListA
    where bs = Ascii.map toUpper bs'

-- | Convert a 'Method' to a 'ByteString'.
methodToByteString :: Method -> Ascii.ByteString
methodToByteString m
    = case m of
        OtherMethod bs -> bs
        _ -> fromMaybe (localError "methodToByteString" "This should not happen (methodListB is incomplete)") $
             lookup m methodListB

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
      , statusMessage :: Ascii.ByteString
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
