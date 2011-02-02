module Network.HTTP.Types
(
  Method
, byteStringToMethod
, methodToByteString
)
where

import           Data.Char
import           Data.Maybe
import qualified Data.ByteString.Char8 as Ascii

localError :: String -> String -> a
localError f s = error $ "Network.HTTP.Types." ++ f ++ ": " ++ s

-- | HTTP method.
-- Note that the Show instance is only for debugging and should NOT be used to generate HTTP method strings. Use 'methodToByteString' instead.
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

-- | 
byteStringToMethod :: Ascii.ByteString -> Method
byteStringToMethod bs' = fromMaybe (OtherMethod bs) $ lookup bs methodListA
    where bs = Ascii.map toUpper bs'

methodToByteString :: Method -> Ascii.ByteString
methodToByteString m
    = case m of
        OtherMethod bs -> bs
        _ -> fromMaybe (localError "methodToByteString" "This should not happen (methodListB is incomplete)") $
             lookup m methodListB

