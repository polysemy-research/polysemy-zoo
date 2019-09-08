{-# LANGUAGE RecursiveDo, TemplateHaskell #-}
module Polysemy.RevState
 ( -- * Effect
   RevState (..)

   -- * Actions
 , revState
 , revGet
 , revPut
 , revModify

   -- * Interpretations
 , runRevState
 , runLazyRevState
 ) where

import Control.Monad.Fix

import Polysemy
import Polysemy.Fixpoint
import Polysemy.Internal
import Polysemy.Internal.Union

------------------------------------------------------------------------------
-- | A 'Polysemy.State.State' effect for threading state /backwards/ instead
-- of forwards through a computation.
newtype RevState s m a where
  RevState :: (s -> (s, a)) -> RevState s m a

makeSem_ ''RevState


------------------------------------------------------------------------------
-- | Gets the state as sent from the next call to 'revState'
-- \/ 'revPut' \/ 'revModify', use it, and send a new state into the past.
revState :: forall s a r
          . Member (RevState s) r
         => (s -> (s, a))
         -> Sem r a

------------------------------------------------------------------------------
-- | Gets the state as sent from the next call to 'revState' \/ 'revPut'
-- \/ 'revModify'.
revGet :: forall s r
        . Member (RevState s) r
       => Sem r s
revGet = revState $ \s -> (s, s)

------------------------------------------------------------------------------
-- | Sends a new state into the past.
revPut :: forall s r
        . Member (RevState s) r
       => s
       -> Sem r ()
revPut s = revState $ \_ -> (s, ())

------------------------------------------------------------------------------
-- | Gets the state as sent from the next call to 'revState'
-- \/ 'revModify' \/ 'revPut', modify it, and return it into the past.
revModify :: forall s r
           . Member (RevState s) r
          => (s -> s)
          -> Sem r ()
revModify f = revState $ \s -> (f s, ())


------------------------------------------------------------------------------
-- | Run a 'RevState' effect with local state that is propagated /backwards/
-- through the computation, from last action to first.
runRevState :: Member Fixpoint r
            => s
            -> Sem (RevState s ': r) a
            -> Sem r (s, a)
runRevState s =
   (`runRevStateT` s)
  . runRevStateInC

------------------------------------------------------------------------------
-- | Run a 'RevState' effect with local state that is lazily propagated
-- /backwards/ through the computation, from last action to first.
runLazyRevState :: Member Fixpoint r
                => s
                -> Sem (RevState s ': r) a
                -> Sem r (s, a)
runLazyRevState s =
   (`runLazyRevStateT` s)
  . runLazyRevStateInC

newtype RevStateT s m a = RevStateT { runRevStateT :: s -> m (s, a) }
  deriving (Functor)

instance MonadFix m => Applicative (RevStateT s m) where
  pure a = RevStateT $ \s -> pure (s, a)
  ff <*> fa = RevStateT $ \s -> do
    rec
      (s'', f) <- runRevStateT ff s'
      (s',  a) <- runRevStateT fa s
    return (s'', f a)
  fa *> fb = fa >>= \_ -> fb

instance MonadFix m => Monad (RevStateT s m) where
  m >>= f = RevStateT $ \s -> do
    rec
      (s'', a) <- runRevStateT m s'
      (s',  b) <- runRevStateT (f a) s
    return (s'', b)

newtype LazyRevStateT s m a = LazyRevStateT { runLazyRevStateT :: s -> m (s, a) }
  deriving (Functor)

instance MonadFix m => Applicative (LazyRevStateT s m) where
  pure a = LazyRevStateT $ \s -> pure (s, a)
  ff <*> fa = LazyRevStateT $ \s -> do
    rec
      ~(s'', f) <- runLazyRevStateT ff s'
      ~(s',  a) <- runLazyRevStateT fa s
    return (s'', f a)
  fa *> fb = fa >>= \_ -> fb

instance MonadFix m => Monad (LazyRevStateT s m) where
  m >>= f = LazyRevStateT $ \s -> do
    rec
      ~(s'', a) <- runLazyRevStateT m s'
      ~(s',  b) <- runLazyRevStateT (f a) s
    return (s'', b)

runRevStateInC :: Member Fixpoint r
               => Sem (RevState s ': r) a
               -> RevStateT s (Sem r) a
runRevStateInC = usingSem $ \u -> RevStateT $ \s ->
  case decomp u of
    Right (Weaving (RevState f) st _ ex _) ->
      return $ (ex . (<$ st)) <$> f s
    Left g ->
      liftSem $
        weave
          (s, ())
          (uncurry runRevState)
          (Just . snd)
          g

runLazyRevStateInC :: Member Fixpoint r
                   => Sem (RevState s ': r) a
                   -> LazyRevStateT s (Sem r) a
runLazyRevStateInC = usingSem $ \u -> LazyRevStateT $ \s ->
  case decomp u of
    Right (Weaving (RevState f) st _ ex _) ->
      return $ (ex . (<$ st)) <$> f s
    Left g ->
      liftSem $
        weave
          (s, ())
          (uncurry runLazyRevState)
          (Just . snd)
          g
