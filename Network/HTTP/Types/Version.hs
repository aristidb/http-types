{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Network.HTTP.Types.Version
(
  HttpVersion(..)
, renderHttpVersion
, http09
, http10
, http11
, http20
)
where

import Data.Typeable (Typeable)
import Data.Data (Data)
import GHC.Generics (Generic)
import Data.ByteString as B

-- | HTTP Version.
--
-- Note that the Show instance is intended merely for debugging.
data HttpVersion
    = HttpVersion {
        httpMajor :: !Int
      , httpMinor :: !Int
      }
    deriving (Eq, Ord, Typeable, Data, Generic)

instance Show HttpVersion where
    show (HttpVersion major minor) = "HTTP/" ++ show major ++ "." ++ show minor

renderHttpVersion :: HttpVersion -> B.ByteString
renderHttpVersion = B.pack . fmap (fromIntegral . fromEnum) . show

-- | HTTP 0.9
http09 :: HttpVersion
http09 = HttpVersion 0 9

-- | HTTP 1.0
http10 :: HttpVersion
http10 = HttpVersion 1 0

-- | HTTP 1.1
http11 :: HttpVersion
http11 = HttpVersion 1 1

-- | HTTP 2.0
http20 :: HttpVersion
http20 = HttpVersion 2 0
