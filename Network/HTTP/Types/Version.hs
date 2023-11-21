{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Types and constants to describe the HTTP version.
module Network.HTTP.Types.Version (
    HttpVersion (..),
    http09,
    http10,
    http11,
    http20,
) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | HTTP Version.
--
-- Note that the 'Show' instance is intended merely for debugging.
data HttpVersion = HttpVersion
    { httpMajor :: !Int
    , httpMinor :: !Int
    }
    deriving
        ( Eq
        , Ord
        , Typeable
        , -- | @since 0.12.4
          Data
        , -- | @since 0.12.4
          Generic
        )

-- | >>> show http11
-- "HTTP/1.1"
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

-- | HTTP 2.0
--
-- @since 0.10
http20 :: HttpVersion
http20 = HttpVersion 2 0
