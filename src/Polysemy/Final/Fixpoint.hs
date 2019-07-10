module Polysemy.Final.Fixpoint
  (
    module Polysemy.Fixpoint
  , module Polysemy.Final
  , runFixpointFinal
  ) where

import Polysemy
import Polysemy.Final
import Polysemy.Fixpoint

import Control.Monad.Fix

-----------------------------------------------------------------------------
-- | Run a 'Fixpoint' effect through a final 'MonadFix'
runFixpointFinal :: (Member (Final m) r, MonadFix m)
                 => Sem (Fixpoint ': r) a
                 -> Sem r a
runFixpointFinal = interpretFinal $ \case
  Fixpoint f -> do
    f' <- bindS f
    pure $ mfix f'
{-# INLINE runFixpointFinal #-}
