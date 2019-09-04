{-# LANGUAGE TemplateHaskell #-}
module Polysemy.EndState
  (-- * Effect
    EndState(..)

    -- * Actions
  , getEndState

    -- * Interpretations
  , runEndState
  , runEndAtomicState
  ) where

import Polysemy
import Polysemy.Fixpoint
import Polysemy.Reader
import Polysemy.Reader.More

import Polysemy.State
import Polysemy.AtomicState


-----------------------------------------------------------------------------
-- | An effect for getting the end state of a computation in advance.
data EndState s m a where
  GetEndState :: EndState s m s

makeSem ''EndState

-----------------------------------------------------------------------------
-- | Runs an 'EndState' effect by getting the state after the computation
-- has finished, and providing it recursively back to calls of 'getEndState'.
runEndState :: (Member (State s) r, Member Fixpoint r)
            => Sem (EndState s ': r) a
            -> Sem r a
runEndState =
    runReaderFixSem get
  . reinterpret (\GetEndState -> ask)


-----------------------------------------------------------------------------
-- | Like 'runEndState', but for 'AtomicState' rather than 'State'.
runEndAtomicState
  :: (Member (AtomicState s) r, Member Fixpoint r)
  => Sem (EndState s ': r) a
  -> Sem r a
runEndAtomicState =
    runReaderFixSem atomicGet
  . reinterpret (\GetEndState -> ask)
