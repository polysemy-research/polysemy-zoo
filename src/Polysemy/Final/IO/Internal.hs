{-# LANGUAGE TemplateHaskell #-}
module Polysemy.Final.IO.Internal where

import Data.Functor.Compose
import Data.Maybe

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Maybe

import Polysemy
import Polysemy.Final
import Polysemy.Internal
import Polysemy.Internal.Union
import Polysemy.Internal.Strategy

------------------------------------------------------------------------------
-- | Like 'interpretFinal' specialized to 'IO', but also tries very hard
-- to preserve state semantics dependant on the order interpreters are run,
-- adressing the primary issue with 'Final'.
--
-- Semantically, interpreters written using this behave very much as
-- though they were written using 'withLowerToIO'.
-- However, this does not need to spawn an interpreter thread, making
-- it more efficient (but not any more safe.)
--
-- 'interpretFinalGlobal' operates under the assumption that any effectful
-- state which can't be inspected using 'Polysemy.Inspector' can't contain any
-- values. For example, the effectful state for 'Polysemy.runError' is
-- @'Either' e a@. The inspector for this effectful state only fails if the
-- effectful state is a @'Left'@ value, which therefore doesn't contain any
-- values of @a@.
--
-- The assumption holds true for all interpreters featured in polysemy,
-- and is presumably always true for any properly implemented interpreter.
-- 'interpretFinalGlobal' may throw an exception if it is used together with an
-- interpreter that uses 'Polysemy.Internal.Union.weave' improperly.
interpretFinalGlobal
    :: forall e a r
     . Member (Final IO) r
    => (forall x n. e n x -> Strategic IO n x)
    -> Sem (e ': r) a
    -> Sem r a
interpretFinalGlobal f sem = withWeavingToFinal $ \s wv ins -> do
  st  <- newMVar s
  res <- runMaybeT $ runViaFinalGlobal st wv ins f sem
  s'  <- readMVar st
  return (fromMaybe bomb res <$ s')
{-# INLINE interpretFinalGlobal #-}

runViaFinalGlobal :: (Member (Final IO) r, Functor f)
                  => MVar (f ())
                  -> (forall x. f (Sem r x) -> IO (f x))
                  -> (forall x. f x -> Maybe x)
                  -> ( forall x n
                     . e n x
                    -> Strategic IO n x
                     )
                  -> Sem (e ': r) a
                  -> MaybeT IO a
runViaFinalGlobal st wv ins f = usingSem $ \u -> case decomp u of
  Right (Weaving e s' wv' ex ins') ->
    fmap ex $ MaybeT $ fmap getCompose $ runStrategy (f e)
          (Compose (Just s'))
          (  maybe
              (pure (Compose Nothing))
              (  fmap Compose
               . runMaybeT
               . runViaFinalGlobal st wv ins f
               . wv'
              )
           . getCompose
          )
          (getCompose >=> ins')
  Left g -> case prj g of
      Just (Weaving (WithWeavingToFinal wav) s' wv' ex' ins') ->
        MaybeT $ fmap (fmap ex' . getCompose) $
          wav
            (Compose (Just s'))
            (  maybe
                (pure (Compose Nothing))
                ( fmap Compose
                . runMaybeT
                . runViaFinalGlobal st wv ins f
                . wv'
                )
             . getCompose
            )
            (getCompose >=> ins')
      _ -> MaybeT $ mask $ \restore -> do
        -- TODO(KingoftheHomeless): Figure out a solution to polysemy issue #205.
        -- Although we're using a different mechanism, the exact same problem manifests
        -- here.
        s   <- takeMVar st
        res <- restore (wv (liftSem (hoist (interpretFinalGlobal f) g) <$ s))
          `onException` putMVar st s
        putMVar st (() <$ res)
        return $ ins res
{-# INLINE runViaFinalGlobal #-}

bomb :: a
bomb = error
  "interpretFinalGlobal: Uninspectable functorial state \
                        \still carried a result. You're likely using an interpreter \
                        \that uses 'weave' improperly. \
                        \See documentation for more information."
