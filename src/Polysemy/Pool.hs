{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Pool where

import Control.Concurrent.STM
import Control.Monad
import Data.Kind
import GHC.Generics
import Polysemy

-- | A sized resource pool for reusable singlethreaded resources
data Pool re (m :: Type -> Type) a where
  Acquire :: Pool re m re
  Release :: re -> Pool re m ()
  GetQuota :: Pool re m Integer
  GetFree :: Pool re m Integer
  GetInUse :: Pool re m Integer

makeSem_ ''Pool

-- | Acquire a unit of resource. If it's not available in pool, allocate one unit or wait for
-- others to drop their resource (if limit is reached).
acquire :: forall re r. Member (Pool re) r => Sem r re

-- | Release the acquired resource. Though nothing will prevent you from keep using it.
release :: forall re r. Member (Pool re) r => re -> Sem r ()

-- | get the size of the pool
getQuota :: forall re r. Member (Pool re) r => Sem r Integer

-- | get the amount of freely available resources
getFree :: forall re r. Member (Pool re) r => Sem r Integer

-- | get the amount of resources that are being acquired by other threads
getInUse :: forall re r. Member (Pool re) r => Sem r Integer

data PoolState re m
  = PoolState
      { inUse :: TVar Integer,
        free :: TVar Integer,
        quota :: Integer,
        pool :: TChan re,
        make :: m re
      }
  deriving (Generic)

runPoolIO :: Members '[Embed IO, Embed m] r => PoolState re m -> Sem (Pool re ': r) a -> Sem r a
runPoolIO !PoolState {..} = interpret $ \case
  Acquire -> do
    empty <- embed $ atomically $ do
      i <- readTVar inUse
      f <- readTVar free
      when (i + f >= quota) retry
      modifyTVar inUse (+ 1)
      isEmptyTChan pool
    if empty
      then embed make
      else embed $ atomically $ do
        modifyTVar free (\i -> i - 1)
        readTChan pool
  Release r -> do
    embed $ atomically $ do
      modifyTVar free (+ 1)
      modifyTVar inUse (\i -> i - 1)
      writeTChan pool r
  GetQuota -> do
    pure quota
  GetInUse -> do
    embed $ atomically $ readTVar inUse
  GetFree -> do
    embed $ atomically $ readTVar free
