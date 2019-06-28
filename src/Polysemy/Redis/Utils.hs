{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Polysemy.Redis.Utils where

import           Data.Binary (Binary)
import qualified Data.Binary as B
import           Data.Binary.Get (runGet)
import           Data.Binary.Put (runPut)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import           GHC.Exts


newtype Path = Path { getPath :: ByteString }
  deriving (Eq, Ord, Show, IsString)


putForRedis :: Binary a => a -> ByteString
putForRedis = L.toStrict . runPut . B.put


getFromRedis :: Binary a => ByteString -> a
getFromRedis = runGet B.get . L.fromStrict

