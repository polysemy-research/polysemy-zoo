module Polysemy.Final.Fixpoint
  (
    module Polysemy.Fixpoint
  , module Polysemy.Final
  , fixpointToFinal
  ) where

import Data.Maybe

import Polysemy
import Polysemy.Final
import Polysemy.Fixpoint
import Polysemy.Internal.Fixpoint

import Control.Monad.Fix

-----------------------------------------------------------------------------
-- | Run a 'Fixpoint' effect through a final 'MonadFix'
--
-- This can be used as an alternative to 'runFixpoint' or 'runFixpointM'.
--
-- __Note__: 'fixpointToFinal' is subject to the same caveats as 'runFixpoint'.
fixpointToFinal :: (Member (Final m) r, MonadFix m)
                => Sem (Fixpoint ': r) a
                -> Sem r a
fixpointToFinal = interpretFinal $
  \(Fixpoint f) -> do
    f'  <- bindS f
    s   <- getInitialStateS
    ins <- getInspectorS
    pure $ mfix $ \fa -> f' $
      fromMaybe (bomb "fixpointToFinal") (inspect ins fa) <$ s
{-# INLINE fixpointToFinal #-}
