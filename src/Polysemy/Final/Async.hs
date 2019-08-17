module Polysemy.Final.Async
  (
    module Polysemy.Async
  , module Polysemy.Final
  , asyncToIOFinal
  , asyncToIOFinalGlobal
  ) where

import qualified Control.Concurrent.Async as A

import Polysemy
import Polysemy.Async
import Polysemy.Final
import Polysemy.Final.IO

------------------------------------------------------------------------------
-- | Run an 'Async' effect through final 'IO'
--
-- This can be used as an alternative to 'lowerAsync'.
--
-- /Beware/: Effects that aren't interpreted in terms of 'IO'
-- will have local state semantics in regards to 'Async' effects
-- interpreted this way. See 'Final'.
--
-- Notably, unlike 'asyncToIO', this is not consistent with
-- 'Polysemy.State.State' unless 'Polysemy.State.runStateIORef' is used.
-- State that seems like it should be threaded globally throughout the `Async`
-- /will not be./
--
-- Prefer 'asyncToIO' or 'asyncToIOThreadless' unless these are unsafe or
-- inefficient in the context of your application.
--
-- Use 'asyncToIO' or 'asyncToIOFinalGlobal' instead if you need to
-- run pure, stateful interpreters after the interpreter for 'Async'.
asyncToIOFinal :: Member (Final IO) r
               => Sem (Async ': r) a
               -> Sem r a
asyncToIOFinal = interpretFinal $ \case
  Async m -> do
    ins <- getInspectorS
    m'  <- runS m
    liftS $ A.async (inspect ins <$> m')
  Await a -> liftS (A.wait a)
{-# INLINE asyncToIOFinal #-}

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
