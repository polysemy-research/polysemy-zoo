{-# LANGUAGE TemplateHaskell #-}

module Polysemy.SetStore where

import           Data.Foldable
import qualified Data.Set as S
import           Polysemy
import           Polysemy.Alias
import           Polysemy.KVStore


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

