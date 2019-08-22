module Polysemy.Final.MTL
  (
    module Polysemy.Final
  , errorToFinal
  , readerToFinal
  , stateToEmbed
  , writerToFinal
  ) where

import Control.Monad.Error.Class hiding (Error)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class

import Polysemy
import Polysemy.Final
import Polysemy.Error hiding (throw, catch)
import Polysemy.Reader hiding (ask, local)
import Polysemy.State hiding (get, put)
import Polysemy.Writer hiding (tell, listen, pass)

-----------------------------------------------------------------------------
-- | Run an 'Error' effect through a final 'MonadError'
--
-- /Beware/: Effects that aren't interpreted in terms of the final
-- monad will have local state semantics in regards to 'Error' effects
-- interpreted this way. See 'Final'.
errorToFinal :: (Member (Final m) r, MonadError e m)
             => Sem (Error e ': r) a
             -> Sem r a
errorToFinal = interpretFinal $ \case
  Throw e   -> pure $ throwError e
  Catch m h -> do
    m' <- runS m
    h' <- bindS h
    s  <- getInitialStateS
    pure $ m' `catchError` (h' . (<$ s))
{-# INLINE errorToFinal #-}

-----------------------------------------------------------------------------
-- | Run a 'Reader' effect through a final 'MonadReader'
--
-- /Beware/: Effects that aren't interpreted in terms of the final
-- monad will have local state semantics in regards to 'Reader' effects
-- interpreted this way. See 'Final'.
readerToFinal :: (Member (Final m) r, MonadReader i m)
              => Sem (Reader i ': r) a
              -> Sem r a
readerToFinal = interpretFinal $ \case
  Ask       -> liftS ask
  Local f m -> do
    m' <- runS m
    pure $ local f m'
{-# INLINE readerToFinal #-}

-----------------------------------------------------------------------------
-- | Run a 'State' effect in terms of an underlying 'MonadState' instance
--
-- Although this is not a 'Final' interpreter, as the target monad need not
-- actually be the final monad, 'stateToEmbed' still possesses the
-- unusual semantics of interpreters that runs
-- effects by embedding them into another monad.
--
-- /Beware/: Effects that aren't interpreted in terms of the embedded
-- monad will have local state semantics in regards to 'State' effects
-- interpreted this way. See 'Final'.
stateToEmbed :: (Member (Embed m) r, MonadState s m)
             => Sem (State s ': r) a
             -> Sem r a
stateToEmbed = interpret $ \case
  Get   -> embed get
  Put s -> embed (put s)
{-# INLINE stateToEmbed #-}

-----------------------------------------------------------------------------
-- | Run a 'Writer' effect through a final 'MonadWriter'
--
-- /Beware/: Effects that aren't interpreted in terms of the final
-- monad will have local state semantics in regards to 'Writer' effects
-- interpreted this way. See 'Final'.
writerToFinal :: (Member (Final m) r, MonadWriter o m)
              => Sem (Writer o ': r) a
              -> Sem r a
writerToFinal = interpretFinal $ \case
  Tell s    -> liftS (tell s)
  Listen m -> do
    m' <- runS m
    pure $
      (\ ~(s, o) -> (,) o <$> s) <$> listen m'
  Pass m -> do
    m'  <- runS m
    ins <- getInspectorS
    pure $ pass $ do
      t <- m'
      let f = maybe id fst (inspect ins t)
      pure (fmap snd t, f)
{-# INLINE writerToFinal #-}
