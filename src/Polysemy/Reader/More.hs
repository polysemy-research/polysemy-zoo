{-# LANGUAGE RecursiveDo #-}
module Polysemy.Reader.More
  (
    module Polysemy.Reader
    -- * Interpretations
  , runReaderFixSem
  ) where

import Polysemy
import Polysemy.Reader
import Polysemy.Fixpoint

------------------------------------------------------------------------------
-- | Runs a 'Reader' effect by running a monadic action /once/, after the
-- 'Sem' has completed, and then providing the result to each request
-- recursively.
runReaderFixSem :: forall i r a
                 . Member Fixpoint r
                => Sem r i
                -> Sem (Reader i ': r) a
                -> Sem r a
runReaderFixSem m sem = do
  rec
    a <- runReader i sem
    i <- m
  return a
{-# INLINE runReaderFixSem #-}
