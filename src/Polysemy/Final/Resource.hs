module Polysemy.Final.Async
  (
    module Polysemy.Resource
  , module Polysemy.Final
  , runResourceFinal
  ) where

import qualified Control.Exception as X
import           Polysemy
import           Polysemy.Resource
import           Polysemy.Final


------------------------------------------------------------------------------
-- | Run a 'Resource' effect in terms of 'X.bracket' through final 'IO'
--
-- This can be used as an alternative to 'runResourceInIO'
--
-- /Beware/: Effects that aren't interpreted in terms of 'IO'
-- will have local state semantics in regards to 'Resource' effects
-- interpreted this way. See 'interpretFinal'.
--
-- Notably, unlike 'runResourceBase', this is not consistent with
-- 'Polysemy.State.State' unless 'Polysemy.State.runStateInIORef' is used.
-- State that seems like it should be threaded globally throughout the `Bracket`
-- /will not be./
--
-- Prefer 'runResourceBase' unless its unsafe or inefficient in the context of
-- your application.
runResourceFinal :: Member (Final IO) r
                 => Sem (Resource ': r) a
                 -> Sem r a
runResourceFinal = interpretFinal $ \case
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
{-# INLINE runResourceFinal #-}
