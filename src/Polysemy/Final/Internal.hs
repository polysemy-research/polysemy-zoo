{-# LANGUAGE TemplateHaskell #-}
module Polysemy.Final.Internal where

import Data.Functor.Compose
import Data.Maybe

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Maybe

import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Union

-----------------------------------------------------------------------------
-- | An effect for embedding higher-order effects in the final target monad
-- of the effect stack.
--
-- This is very useful for writing interpreters that interpret higher-order
-- effects in terms of the final monad.
--
-- 'Final' is slightly weaker, but using it is otherwise more preferable
-- than using a provided lowering function (a la 'Polysemy.Async.lowerAsync'):
--
--   * Interpreters using 'Final' may be run and composed like any other
--     interpreter, unlike @lower-@ interpreters which need to be
--     composed using '.@' or '.@@'.
--
--   * Multiple return-value changing interpreters using 'Final' may be
--     composed together without issue. It's very difficult to
--     compose 'Polysemy.Error.lowerError' with itself, unlike
--     'Polysemy.Final.Error.errorToIOFinal'.
--
--   * Initialization work of @lower-@ interpreters may be duplicated
--     when composed together with '.@'. @-'Final'@ interpreters avoid
--     this issue altogether, as long as the initialization work is performed
--     outside of a 'interpretH'/'interpretFinal'.
--
--   * Instead of having access to a natural transformation
--     @forall x. 'Sem' r x -> m x@, @'Final' m@ provides a
--     distribution through the effectful state:
--     @forall x. f ('Sem' r x) -> m (f x)@,
--     together with the initial state @f ()@, and an @'Inspector' f@.
--     This is powerful enough for most purposes.
--
-- /Beware/: 'Final' actions are interpreted as actions of the final monad,
-- and the effectful state visible to
-- 'withWeaving'\/'Polysemy.Final.withStrategic'\/'Polysemy.Final.interpretFinal'
-- is that of /all interpreters run in order to produce the final monad/.
--
-- This means that any interpreter built using 'Final' will /not/
-- respect local/global state semantics based on the order of
-- interpreters run. You should signal interpreters that make use of
-- 'Final' by adding a @-'Final'@ suffix to the names of these.
--
-- State semantics of effects that are /not/
-- interpreted in terms of the final monad will always
-- appear local to effects that are interpreted in terms of the final monad.
--
-- State semantics between effects that are interpreted in terms of the final monad
-- depend on the final monad. For example, if the final monad is a monad transformer
-- stack, then state semantics will depend on the order monad transformers are stacked.
newtype Final m z a where
  WithWeaving :: (  forall f
                  . Functor f
                 => f ()
                 -> (forall x. f (z x) -> m (f x))
                 -> (forall x. f x -> Maybe x)
                 -> m (f a)
                 )
              -> Final m z a

makeSem_ ''Final

-----------------------------------------------------------------------------
-- | Allows for embedding higher-order actions of the final monad
-- by providing the means of explicitly threading effects through @'Sem' r@
-- to the final monad.
--
-- Consider using 'Polysemy.Final.withStrategic' instead,
-- which provides a more user-friendly interface, but is also slightly weaker.
--
-- You are discouraged from using 'withWeaving' directly in application code,
-- as it ties your application code directly to the final monad.
withWeaving :: forall m a r
             . Member (Final m) r
            => (  forall f
                . Functor f
               => f ()
               -> (forall x. f (Sem r x) -> m (f x))
               -> (forall x. f x -> Maybe x)
               -> m (f a)
               )
            -> Sem r a

data Strategy m f n z a where
  GetInitialState     :: Strategy m f n z (f ())
  HoistInterpretation :: (a -> n b) -> Strategy m f n z (f a -> m (f b))
  GetInspector        :: Strategy m f n z (Inspector f)

------------------------------------------------------------------------------
-- | 'Strategic' is an environment in which you're capable of explicitly
-- threading higher-order effect states to the final monad.
-- This is a variant of @Tactics@ (see 'Tactical'), and usage
-- is extremely similar.
type Strategic m n a = forall f. Functor f => Sem (WithStrategy m f n) (m (f a))

type WithStrategy m f n = '[Strategy m f n]

------------------------------------------------------------------------------
-- | Internal function to process Strategies in terms of 'withWeaving'.
runStrategy :: Functor f
            => f ()
            -> (forall x. f (n x) -> m (f x))
            -> (forall x. f x -> Maybe x)
            -> Sem '[Strategy m f n] a
            -> a
runStrategy s wv ins = (run .) $ interpret $ \case
  GetInitialState       -> pure s
  HoistInterpretation f -> pure $ \fa -> wv (f <$> fa)
  GetInspector          -> pure (Inspector ins)
{-# INLINE runStrategy #-}


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
interpretFinalGlobal f sem = withWeaving $ \s wv ins -> do
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
    fmap ex $ MaybeT $ fmap getCompose $ runStrategy
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
          (f e)
  Left g -> case prj g of
      Just (Weaving (WithWeaving wav) s' wv' ex' ins') ->
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
