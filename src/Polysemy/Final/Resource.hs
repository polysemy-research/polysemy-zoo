module Polysemy.Final.Resource
  (
    module Polysemy.Resource
  , module Polysemy.Final
  , resourceToIOFinal
  , resourceToIOFinalGlobal
  ) where

import qualified Control.Exception as X

import           Polysemy
import           Polysemy.Resource
import           Polysemy.Final
import           Polysemy.Final.IO


------------------------------------------------------------------------------
-- | Run a 'Resource' effect in terms of 'X.bracket' through final 'IO'
--
-- This is a better alternative of 'lowerResource'
--
-- /Beware/: Effects that aren't interpreted in terms of 'IO'
-- will have local state semantics in regards to 'Resource' effects
-- interpreted this way. See 'Final'.
--
-- Notably, unlike 'resourceToIO', this is not consistent with
-- 'Polysemy.State.State' unless 'Polysemy.State.runStateInIORef' is used.
-- State that seems like it should be threaded globally throughout 'bracket's
-- /will not be./
--
-- Use 'resourceToIO' or 'resourceToIOFinalGlobal' instead if you need to
-- run pure, stateful interpreters after the interpreter for 'Resource'.
resourceToIOFinal :: Member (Final IO) r
                  => Sem (Resource ': r) a
                  -> Sem r a
resourceToIOFinal = interpretFinal $ \case
  Bracket alloc dealloc use -> do
    a <- runS  alloc
    d <- bindS dealloc
    u <- bindS use
    pure $ X.bracket a d u

  BracketOnError alloc dealloc use -> do
    a <- runS  alloc
    d <- bindS dealloc
    u <- bindS use
    pure $ X.bracketOnError a d u
{-# INLINE resourceToIOFinal #-}


------------------------------------------------------------------------------
-- | 'resourceToIOFinal' implemented using 'interpretFinalGlobal'.
--
-- This behaves semantically very much like 'resourceToIO',
-- but doesn't need to spin up an interpreter thread,
-- making it more efficient (but not any more safe).
resourceToIOFinalGlobal :: Member (Final IO) r
                        => Sem (Resource ': r) a
                        -> Sem r a
resourceToIOFinalGlobal = interpretFinalGlobal $ \case
  Bracket alloc dealloc use -> do
    a <- runS  alloc
    d <- bindS dealloc
    u <- bindS use
    pure $ X.bracket a d u

  BracketOnError alloc dealloc use -> do
    a <- runS  alloc
    d <- bindS dealloc
    u <- bindS use
    pure $ X.bracketOnError a d u
{-# INLINE resourceToIOFinalGlobal #-}
