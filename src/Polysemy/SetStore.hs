{-# LANGUAGE TemplateHaskell #-}

module Polysemy.SetStore where

import           Control.Monad
import           Data.Binary (Binary)
import           Data.Foldable
import qualified Data.Set as S
import qualified Database.Redis as R
import           Polysemy
import           Polysemy.Alias
import           Polysemy.Error
import           Polysemy.KVStore
import           Polysemy.Redis.Utils



data SetStore k v m a where
  AddS :: k -> v -> SetStore k v m ()
  DelS :: k -> v -> SetStore k v m ()
  MemberS :: k -> v -> SetStore k v m Bool

makeSem ''SetStore


runSetStoreAsKVStore
    :: ( Member (KVStore k (S.Set v)) r
       , Ord v
       )
    => InterpreterOf (SetStore k v) r
runSetStoreAsKVStore = interpret $ \case
  AddS k v ->
    lookupKV k >>= \case
      Just s  -> writeKV k $ S.insert v s
      Nothing -> writeKV k $ S.singleton v
  DelS k v -> do
    ms <- lookupKV k
    for_ ms $ writeKV k . S.delete v
  MemberS k v ->
    pure . maybe False (S.member v) =<< lookupKV k
{-# INLINE runSetStoreAsKVStore #-}


runSetStoreInRedis
    :: ( Member (Embed R.Redis) r
       , Member (Error R.Reply) r
       , Binary k
       , Binary v
       )
    => (k -> Path)
    -> InterpreterOf (SetStore k v) r
runSetStoreInRedis pf = interpret $ \case
  AddS k v -> void
            . fromEitherM
            . R.sadd (getPath $ pf k)
            . pure
            $ putForRedis v
  DelS k v -> void
            . fromEitherM
            . R.srem (getPath $ pf k)
            . pure
            $ putForRedis v
  MemberS k v -> fromEitherM
               . R.sismember (getPath $ pf k)
               $ putForRedis v
{-# INLINE runSetStoreInRedis #-}

