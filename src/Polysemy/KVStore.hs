{-# LANGUAGE TemplateHaskell #-}

module Polysemy.KVStore
  ( -- * Effect
    KVStore (..)

    -- * Actions
  , lookupKV
  , lookupOrThrowKV
  , existsKV
  , writeKV
  , deleteKV
  , updateKV
  , modifyKV

    -- * Interpretations
  , runKVStoreAsState
  , runKVStorePurely
  , runKVStoreInRedis
  ) where

import           Control.Monad
import           Data.Binary (Binary)
import           Data.ByteString (ByteString)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import qualified Database.Redis as R
import           Polysemy
import           Polysemy.Error
import           Polysemy.Redis.Utils
import           Polysemy.State


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


------------------------------------------------------------------------------
-- |
--
-- @since 0.3.1.0
lookupOrThrowKV
    :: Members '[ KVStore k v
                , Error e
                ] r
    => (k -> e)
    -> k
    -> Sem r v
lookupOrThrowKV f k =
  fromEither . maybe (Left $ f k) Right =<< lookupKV k


------------------------------------------------------------------------------
-- |
--
-- @since 0.3.1.0
existsKV :: Member (KVStore k v) r => k -> Sem r Bool
existsKV = fmap isJust . lookupKV


------------------------------------------------------------------------------
-- |
--
-- @since 0.3.1.0
modifyKV
    :: Member (KVStore k v) r
    => v  -- ^ Default value if the key isn't present
    -> (v -> v)
    -> k
    -> Sem r ()
modifyKV d f k =
  lookupKV k >>= \case
    Just v  -> writeKV k $ f v
    Nothing -> writeKV k $ f d


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


runKVStoreInRedis
    :: ( Member (Lift R.Redis) r
       , Member (Error R.Reply) r
       , Binary k
       , Binary v
       )
    => (k -> ByteString)
    -> Sem (KVStore k v ': r) a
    -> Sem r a
runKVStoreInRedis pf = interpret $ \case
  LookupKV k -> do
    res <- fromEitherM $ R.hget (pf k) $ putForRedis k
    pure $ fmap getFromRedis res

  UpdateKV k Nothing ->
    void . fromEitherM
         . R.hdel (pf k)
         . pure
         $ putForRedis k

  UpdateKV k (Just v) ->
    void . fromEitherM
         . R.hset (pf k) (putForRedis k)
         $ putForRedis v
{-# INLINE runKVStoreInRedis #-}

