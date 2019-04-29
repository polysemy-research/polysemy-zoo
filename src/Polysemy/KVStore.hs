{-# LANGUAGE TemplateHaskell #-}

module Polysemy.KVStore
  ( -- * Effect
    KVStore (..)

    -- * Actions
  , lookupKV
  , writeKV
  , deleteKV
  , updateKV

    -- * Interpretations
  , runKVStoreAsState
  , runKVStorePurely
  ) where

import Polysemy
import Polysemy.State
import qualified Data.Map as M


------------------------------------------------------------------------------
-- | Models things like Redis, HTTP GET/POST, etc. Things that are keyed, have
-- a value, and may or may not be there.
data KVStore k v m a where
  LookupKV :: k -> KVStore k v m (Maybe v)
  UpdateKV :: k -> Maybe v -> KVStore k v m ()

makeSem ''KVStore


writeKV :: Member (KVStore k v) r => k -> v -> Sem r ()
writeKV k = updateKV k . Just
{-# INLINE writeKV #-}


deleteKV :: Member (KVStore k v) r => k -> Sem r ()
deleteKV k = updateKV k Nothing
{-# INLINE deleteKV #-}


runKVStoreAsState :: Ord k => Sem (KVStore k v ': r) a -> Sem (State (M.Map k v) ': r) a
runKVStoreAsState = reinterpret $ \case
  LookupKV k   -> gets $ M.lookup k
  UpdateKV k v -> modify $ M.alter (const v) k
{-# INLINE runKVStoreAsState #-}


runKVStorePurely
    :: Ord k
    => M.Map k v
    -> Sem (KVStore k v ': r) a
    -> Sem r (M.Map k v, a)
runKVStorePurely m = runState m . runKVStoreAsState
{-# INLINE runKVStorePurely #-}

