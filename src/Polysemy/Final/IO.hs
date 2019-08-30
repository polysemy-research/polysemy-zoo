module Polysemy.Final.IO
  (
    -- * Combinators for Interpreting to the Final Monad
    interpretFinalGlobal

    -- * Interpretations for other effects
  , asyncToIOFinalGlobal
  , resourceToIOFinalGlobal
  ) where

import qualified Control.Concurrent.Async as A
import qualified Control.Exception as X

import Polysemy
import Polysemy.Final
import Polysemy.Final.IO.Internal
import Polysemy.Async
import Polysemy.Resource

------------------------------------------------------------------------------
-- | 'asyncToIOFinal' implemented using 'interpretFinalGlobal'.
--
-- This behaves semantically very much like 'asyncToIO',
-- but doesn't need to spin up an interpreter thread, making it more
-- efficient (but not any more safe).
asyncToIOFinalGlobal :: Member (Final IO) r
                     => Sem (Async ': r) a
                     -> Sem r a
asyncToIOFinalGlobal = interpretFinalGlobal $ \case
  Async m -> do
    ins <- getInspectorS
    m'  <- runS m
    liftS $ A.async (inspect ins <$> m')
  Await a -> liftS (A.wait a)
{-# INLINE asyncToIOFinalGlobal #-}
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
