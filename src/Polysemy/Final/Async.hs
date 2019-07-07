module Polysemy.Final.Async
  (
    runAsyncFinal
  ) where

import qualified Control.Concurrent.Async as A

import Polysemy
import Polysemy.Async
import Polysemy.Final

------------------------------------------------------------------------------
-- | Run an 'Async' effect through final 'IO'
--
-- Unlike 'runAsync', this is not consistent with 'Polysemy.State.State'
-- unless 'runStateInIORef' is used. Use 'runAsyncFinal' only
-- if 'runAsync' is unsafe in the context of your application.
runAsyncFinal :: Member (Final IO) r
              => Sem (Async ': r) a
              -> Sem r a
runAsyncFinal = interpretHFinal $ \case
  Async m -> do
    ins <- getInspectorS
    m' <- runS m
    liftS $ A.async (inspect ins <$> m')
  Await a -> liftS (A.wait a)
