{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Floodgate
  ( -- * Effect
    Floodgate (..)
    -- * Actions
  , hold
  , release

    -- * Interpretations
  , runFloodgate
  , runFloodgateDry
  ) where

import Control.Monad
import GHC.Types
import Polysemy
import Polysemy.State
import Unsafe.Coerce

------------------------------------------------------------------------------
-- |
--
-- @since 0.3.1.0
data Floodgate m a where
  Hold    :: m () -> Floodgate m ()
  Release :: Floodgate m ()

makeSem ''Floodgate


------------------------------------------------------------------------------
-- |
--
-- @since 0.3.1.0
runFloodgate
    :: Sem (Floodgate ': r) a
    -> Sem r a
runFloodgate = fmap snd . runState @[Any] [] . reinterpretH
  ( \case
      Hold m -> do
        m' <- fmap void $ runT m
        -- These 'Any's are here because the monadic action references 'r', and
        -- if we exposed that, 'r' would be an infinite type
        modify (unsafeCoerce @_ @Any (raise $ runFloodgate m') :)
        getInitialStateT

      Release -> do
        ms' <- gets (fmap unsafeCoerce . reverse)
        sequence_ ms'
        getInitialStateT
  )


------------------------------------------------------------------------------
-- | Like 'runFloodgate', but will do a final flush to 'release' anything that
-- might still be behind the floodgate.
--
-- @since 0.3.1.0
runFloodgateDry
    :: Sem (Floodgate ': r) a
    -> Sem r a
runFloodgateDry m = runFloodgate $ m <* release

