{-# LANGUAGE FlexibleInstances #-}

-- | Some type classes to make more general functions when handling query strings.
module Network.HTTP.Types.QueryLike (
    QueryLike (..),
    QueryKeyLike (..),
    QueryValueLike (..),
)
where

import Control.Arrow ((***))
import Data.ByteString as B (ByteString, concat)
import Data.ByteString.Lazy as L (ByteString, toChunks)
import Data.Maybe (catMaybes)
import Data.Text as T (Text, pack)
import Data.Text.Encoding as T (encodeUtf8)

import Network.HTTP.Types.URI (Query)

-- | Types which can, and commonly are, converted to 'Query' are in this class.
--
-- You can use lists of simple key value pairs, with 'B.ByteString' (strict, or lazy:
-- 'L.ByteString'), 'T.Text', or 'String' as the key/value types. You can also have the value
-- type lifted into a Maybe to support keys without values; and finally it is possible to put
-- each pair into a Maybe for key-value pairs that aren't always present.
--
-- @since 0.7.0
class QueryLike a where
    -- | Convert to 'Query'.
    toQuery :: a -> Query

-- | Types which, in a Query-like key-value list, are used in the Key position.
class QueryKeyLike a where
    toQueryKey :: a -> B.ByteString

-- | Types which, in a Query-like key-value list, are used in the Value position.
class QueryValueLike a where
    toQueryValue :: a -> Maybe B.ByteString

-- | @since 0.7.0
instance (QueryKeyLike k, QueryValueLike v) => QueryLike [(k, v)] where
    toQuery = map (toQueryKey *** toQueryValue)

-- | @since 0.7.0
instance (QueryKeyLike k, QueryValueLike v) => QueryLike [Maybe (k, v)] where
    toQuery = toQuery . catMaybes

-- | @since 0.7.0
instance QueryKeyLike B.ByteString where toQueryKey = id

-- | @since 0.7.0
instance QueryKeyLike L.ByteString where toQueryKey = B.concat . L.toChunks

-- | @since 0.7.0
instance QueryKeyLike T.Text where toQueryKey = T.encodeUtf8

-- | @since 0.7.0
instance QueryKeyLike [Char] where toQueryKey = T.encodeUtf8 . T.pack

-- | @since 0.7.0
instance QueryValueLike B.ByteString where toQueryValue = Just

-- | @since 0.7.0
instance QueryValueLike L.ByteString where toQueryValue = Just . B.concat . L.toChunks

-- | @since 0.7.0
instance QueryValueLike T.Text where toQueryValue = Just . T.encodeUtf8

-- | @since 0.7.0
instance QueryValueLike [Char] where toQueryValue = Just . T.encodeUtf8 . T.pack

-- | @since 0.7.0
instance QueryValueLike a => QueryValueLike (Maybe a) where
    toQueryValue = maybe Nothing toQueryValue
