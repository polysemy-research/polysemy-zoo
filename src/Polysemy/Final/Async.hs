module Polysemy.Final.Async
  (
    module Polysemy.Async
  , module Polysemy.Final
  , runAsyncFinal
  ) where

import qualified Control.Concurrent.Async as A

import Polysemy
import Polysemy.Async
import Polysemy.Final

------------------------------------------------------------------------------
-- | Run an 'Async' effect through final 'IO'
--
-- This can be used as an alternative to 'runAsyncInIO'.
--
-- /Beware/: Effects that aren't interpreted in terms of 'IO'
-- will have local state semantics in regards to 'Async' effects
-- interpreted this way. See 'interpretFinal'.
--
-- Notably, unlike 'runAsync', this is not consistent with
-- 'Polysemy.State.State' unless 'Polysemy.State.runStateInIORef' is used.
-- State that seems like it should be threaded globally throughout the `Async`
-- /will not be./
--
-- Prefer 'runAsync' unless its unsafe or inefficient in the context of your
-- application.
runAsyncFinal :: Member (Final IO) r
              => Sem (Async ': r) a
              -> Sem r a
runAsyncFinal = interpretFinal $ \case
  Async m -> do
    ins <- getInspectorS
    m' <- runS m
    liftS $ A.async (inspect ins <$> m')
  Await a -> liftS (A.wait a)
