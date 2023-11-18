{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.HTTP.Types.Method (
    Method,
    methodGet,
    methodPost,
    methodHead,
    methodPut,
    methodDelete,
    methodTrace,
    methodConnect,
    methodOptions,
    methodPatch,
    StdMethod (..),
    parseMethod,
    renderMethod,
    renderStdMethod,
)
where

import Control.Arrow ((|||))
import Data.Array
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | HTTP method (flat 'ByteString' type).
type Method = B.ByteString

-- | HTTP GET Method
methodGet,
    -- | HTTP POST Method
    methodPost,
    -- | HTTP HEAD Method
    methodHead,
    -- | HTTP PUT Method
    methodPut,
    -- | HTTP DELETE Method
    methodDelete,
    -- | HTTP TRACE Method
    methodTrace,
    -- | HTTP CONNECT Method
    methodConnect,
    -- | HTTP OPTIONS Method
    methodOptions,
    -- | HTTP PATCH Method
    --
    -- @since 0.8.0
    methodPatch :: Method
methodGet     = renderStdMethod GET
methodPost    = renderStdMethod POST
methodHead    = renderStdMethod HEAD
methodPut     = renderStdMethod PUT
methodDelete  = renderStdMethod DELETE
methodTrace   = renderStdMethod TRACE
methodConnect = renderStdMethod CONNECT
methodOptions = renderStdMethod OPTIONS
methodPatch = renderStdMethod PATCH

-- | HTTP standard method (as defined by RFC 2616, and PATCH which is defined
--   by RFC 5789).
--
-- @since 0.2.0
data StdMethod
    = GET
    | POST
    | HEAD
    | PUT
    | DELETE
    | TRACE
    | CONNECT
    | OPTIONS
    | PATCH -- ^ @since 0.8.0
    deriving (
        Read,
        Show,
        Eq,
        Ord,
        Enum,
        Bounded,
        Ix,
        Typeable,
        Generic,
        -- ^ @since 0.12.4
        Data
        -- ^ @since 0.12.4
    )
-- These are ordered by suspected frequency. More popular methods should go first.
-- The reason is that methodList is used with lookup.
-- lookup is probably faster for these few cases than setting up an elaborate data structure.

-- FIXME: listArray (minBound, maxBound) $ fmap fst methodList
methodArray :: Array StdMethod Method
methodArray = listArray (minBound, maxBound) $ map (B8.pack . show) [minBound :: StdMethod .. maxBound]

-- FIXME: map (\m -> (B8.pack $ show m, m)) [minBound .. maxBound]
methodList :: [(Method, StdMethod)]
methodList = map (\(a, b) -> (b, a)) (assocs methodArray)

-- | Convert a method 'ByteString' to a 'StdMethod' if possible.
--
-- @since 0.2.0
parseMethod :: Method -> Either B.ByteString StdMethod
parseMethod bs = maybe (Left bs) Right $ lookup bs methodList

-- | Convert an algebraic method to a 'ByteString'.
--
-- prop> renderMethod (parseMethod bs) == bs
--
-- @since 0.3.0
renderMethod :: Either B.ByteString StdMethod -> Method
renderMethod = id ||| renderStdMethod

-- | Convert a 'StdMethod' to a 'ByteString'.
--
-- @since 0.2.0
renderStdMethod :: StdMethod -> Method
renderStdMethod m = methodArray ! m
