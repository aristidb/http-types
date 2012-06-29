{-# LANGUAGE FlexibleInstances #-}
module Network.HTTP.Types.QueryLike where

import           Network.HTTP.Types
import           Data.Maybe
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Control.Arrow

class QueryLike a where
  toQuery :: a -> Query

class QueryKeyLike a where
  toQueryKey :: a -> B.ByteString

class QueryValueLike a where
  toQueryValue :: a -> Maybe B.ByteString

instance (QueryKeyLike k, QueryValueLike v) => QueryLike [(k, v)] where
  toQuery = map (toQueryKey *** toQueryValue)

instance (QueryKeyLike k, QueryValueLike v) => QueryLike [Maybe (k, v)] where
  toQuery = toQuery . catMaybes

instance QueryKeyLike B.ByteString where toQueryKey = id
instance QueryKeyLike L.ByteString where toQueryKey = B.concat . L.toChunks
instance QueryKeyLike T.Text where toQueryKey = T.encodeUtf8
instance QueryKeyLike String where toQueryKey = T.encodeUtf8 . T.pack

instance QueryValueLike B.ByteString where toQueryValue = Just
instance QueryValueLike L.ByteString where toQueryValue = Just . B.concat . L.toChunks
instance QueryValueLike T.Text where toQueryValue = Just . T.encodeUtf8
instance QueryValueLike String where toQueryValue = Just . T.encodeUtf8 . T.pack

instance QueryValueLike a => QueryValueLike (Maybe a) where
  toQueryValue = maybe Nothing toQueryValue