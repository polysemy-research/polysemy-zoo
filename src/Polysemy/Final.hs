{-# LANGUAGE TemplateHaskell #-}
module Polysemy.Final
  (
    -- * Effect
    Final(..)

    -- * Actions
  , withWeaving
  , withStrategic
  , sendM'

    -- * Combinators for Interpreting to the Final Monad
  , interpretHFinal

    -- * Strategy
    -- | Strategy is a domain-specific language very similar to @Tactics@
    -- (see 'Tactical'), and is used to describe how higher-order effects
    -- are threaded
    -- down to the final monad.
    --
    -- Much like @Tactics@, computations can be run and threaded
    -- through the use of 'runS' and 'bindS', and first-order constructors
    -- may use 'pureS'.
    --
    -- Unlike @Tactics@, the final return value within a `Strategic`
    -- must be a monadic value of the target monad
    -- with the functorial state wrapped inside of it.
  , Strategic
  , WithStrategy
  , pureS
  , runS
  , bindS
  , getInspectorS
  , getInitialStateS
  , runStrategy

    -- * Interpretations
  , runFinal
  , runFinalLift
  , runFinalLiftIO
  ) where

import Data.Functor.Identity

import Polysemy
import Data.Functor.Compose
import Polysemy.Internal
import Polysemy.Internal.Tactics
import Polysemy.Internal.Union
import Control.Monad
import Control.Monad.IO.Class

-----------------------------------------------------------------------------
-- | An effect for embedding higher-order effects in the final target monad
-- of the effect stack.
--
-- This is very useful for writing interpreters that interpret higher-order
-- effects in terms of the final monad - however, these interpreters
-- are subject to very different semantics than regular ones.
-- For more information, see ''interpretHFinal'.
data Final m z a where
  WithWeaving :: (forall f.
                      Functor f
                   => f ()
                   -> (forall x. f (z x) -> m (f x))
                   -> (forall x. f x -> Maybe x)
                   -> m (f a)
                 )
              -> Final m z a

makeSem_ ''Final

-----------------------------------------------------------------------------
-- | Allows for embedding higher-order actions of the final monad
-- by providing the means of explicitly threading effects through 'Sem r'
-- to the final monad. Consider using 'withStrategic' instead,
-- as it provides a more user-friendly interface to the same power.
--
-- You are discouraged from using 'withWeaving' directly in application code,
-- as it ties your application code directly to the underlying result.
withWeaving :: forall m a r
            .   Member (Final m) r
            => (forall f.
                    Functor f
                 => f ()
                 -> (forall x. f (Sem r x) -> m (f x))
                 -> (forall x. f x -> Maybe x)
                 -> m (f a)
               )
            -> Sem r a

-----------------------------------------------------------------------------
-- | 'withWeaving' admits an implementation of 'sendM'
-- by providing the means of threading effects through 'Sem r x'
-- to the final monad.
--
-- Just like 'sendM', you are discouraged from using this in application code.
sendM' :: Monad m => Member (Final m) r => m a -> Sem r a
sendM' m = withWeaving $ \s _ _ -> (<$ s) <$> m


-----------------------------------------------------------------------------
-- | Allows for embedding higher-order actions of the final monad
-- by providing the means of explicitly threading effects through 'Sem r'
-- to the final monad. This is done through the use of the 'Strategic'
-- environment.
--
-- You are discouraged from using 'withStrategic' directly in application code,
-- as it ties your application code directly to the underlying result.
withStrategic :: Member (Final m) r => Strategic m (Sem r) a -> Sem r a
withStrategic strat = withWeaving $ \s wv ins -> runStrategy s wv ins strat

------------------------------------------------------------------------------
-- | Like 'interpretH', but may be used to
-- interpret higher-order effects in terms of the final monad.
--
-- _Beware_: Any interpreters built using this (or 'Final' in general)
-- will _not_ respect local/global state semantics based on the order of
-- interpreters run. You are therefore encouraged to use the "-Final" suffix
-- when naming interpreters that make use of 'Final', in order to signal that
-- fact.
--
-- State semantics of effects that are _not_
-- interpreted in terms of the final monad will always
-- appear local to effects that are interpreted in terms of the final monad.
--
-- State semantics between effects that are interpreted in terms of the final monad
-- depend on the final monad. I.e. if the final monad is a monad transformer stack,
-- then state semantics will depend on the order monad transformers are stacked.
interpretHFinal
    :: forall e m r a
    .  (Member (Final m) r, Monad m)
    => (forall x n. e n x -> Strategic m n x)
    -> Sem (e ': r) a
    -> Sem r a
interpretHFinal n =
  let
    go :: Sem (e ': r) x -> Sem r x
    go (Sem sem) = sem $ \u -> case decomp u of
      Right (Yo e s wv ex ins) ->
        fmap ex $ withWeaving $ \s' wv' ins'
          -> fmap getCompose $
                runStrategy
                  (Compose (s <$ s'))
                  (fmap Compose . wv' . fmap (go_b . wv) . getCompose)
                  (ins' . getCompose >=> ins)
                  (n e)
      Left g -> liftSem (hoist go_b g)
    {-# INLINE go #-}

    go_b :: Sem (e ': r) x -> Sem r x
    go_b = go
    {-# NOINLINE go_b #-}
  in
    go
{-# INLINE interpretHFinal #-}

------------------------------------------------------------------------------
-- | 'Strategic' is an environment in which you're capable of explicitly
-- threading higher-order effect states to the final monad.
-- This is based upon @Tactics@, (see 'Tactical'), and usage
-- is extremely similar.
type Strategic m n a = forall f. Functor f => Sem (WithStrategy m f n) (m (f a))

type WithStrategy m f n = WithTactics (Lift m) f n '[]

------------------------------------------------------------------------------
-- | Get a natural transformation capable of potentially inspecting values
-- inside of @f@. Binding the result of 'getInspectorS' produces a function that
-- can sometimes peek inside values returned by 'bindS'.
--
-- This is often useful for running callback functions that are not managed by
-- polysemy code.
--
-- See also 'getInspectorT'
getInspectorS :: Sem (WithStrategy m f n) (Inspector f)
getInspectorS = getInspectorT
{-# INLINE getInspectorS #-}

-- | Get the stateful environment of the world at the moment the
-- target monad is to be run.
-- Prefer 'pureS', 'runS' or 'bindS' instead of using this function
-- directly.
getInitialStateS :: Sem (WithStrategy m f n) (f ())
getInitialStateS = getInitialStateT
{-# INLINE getInitialStateS #-}

------------------------------------------------------------------------------
-- Lift a value into 'Strategic'.
pureS :: Monad m => a -> Strategic m n a
pureS a = do
  s <- getInitialStateS
  pure (pure (a <$ s))
{-# INLINE pureS #-}

------------------------------------------------------------------------------
-- | Lifts a monadic action into the stateful environment, in terms
-- of the final monad.
-- The stateful environment will be the same as the one that the target monad
-- is initially run in.
-- Use 'bindS'  if you'd prefer to explicitly manage your stateful environment.
runS :: Monad m => n a -> Sem (WithStrategy m f n) (m (f a))
runS = fmap runM . runT
{-# INLINE runS #-}

------------------------------------------------------------------------------
-- | Lift a kleisli action into the stateful environment, in terms of the final
-- monad. You can use 'bindS' to get an effect parameter of the form @a -> m b@
-- into something that can be used after calling 'runS' on an effect parameter
-- @m a@.
bindS :: Monad m => (a -> n b) -> Sem (WithStrategy m f n) (f a -> m (f b))
bindS = fmap (runM .) . bindT
{-# INLINE bindS #-}

------------------------------------------------------------------------------
-- | Internal function to process Strategies in terms of 'withWeaving'.
runStrategy :: Functor f
            => f ()
            -> (forall x. f (n x) -> m (f x))
            -> (forall x. f x -> Maybe x)
            -> Sem (WithStrategy m f n) a
            -> a
runStrategy s wv ins (Sem m) = runIdentity $ m $ \u -> case extract u of
  Yo e s' _ ex' _ -> Identity $ ex' $ (<$ s') $ case e of
    GetInitialState -> s
    HoistInterpretation na -> sendM . wv . fmap na
    GetInspector -> Inspector ins

------------------------------------------------------------------------------
-- Lower a 'Sem' containing only a lifted, final monad into that monad.
-- The appearance of 'Lift' as the final effect
-- is to allow the use of operations that rely on a @'LastMember' ('Lift' m)@
-- constraint.
runFinal :: Monad m => Sem '[Final m, Lift m] a -> m a
runFinal (Sem sem) = sem $ \u -> case decomp u of
  Right (Yo (WithWeaving wav) s wv ex ins) ->
    ex <$> wav s (runFinal . wv) ins
  Left g -> case extract g of
    Yo (Lift m) s _ ex _ -> ex . (<$ s) <$> m

------------------------------------------------------------------------------
-- Lower a 'Sem' containing two lifted monad into the final monad,
-- by interpreting one of the monads in terms of the other one.
--
-- This allows for the use of operations that rely on a @'LastMember' ('Lift' m)@
-- constraint, as long as @m@ can be transformed to the final monad;
-- but be warned, this breaks the implicit contract of @'LastMember' ('Lift' m)@
-- that @m@ truly _is_ the final monad, so depending on the final monad and operations
-- used, 'runFinalTrans' may become _unsafe_.
--
-- For example, 'runFinalTrans' is unsafe with 'runAsync' if
-- the final monad is non-deterministic, or a continuation
-- monad.
runFinalLift :: Monad m
              => (forall x. n x -> m x)
              -> Sem [Final m, Lift m, Lift n] a
              -> m a
runFinalLift nat (Sem sem) = sem $ \u -> case decomp u of
  Right (Yo (WithWeaving wav) s wv ex ins) ->
    ex <$> wav s (runFinalLift nat . wv) ins
  Left g -> case decomp g of
    Right (Yo (Lift m) s _ ex _) -> ex . (<$ s) <$> m
    Left g' -> case extract g' of
      Yo (Lift n) s _ ex _ -> ex . (<$ s) <$> nat n

------------------------------------------------------------------------------
-- | 'runFinalTrans', specialized to transform 'IO' to a 'MonadIO'.
runFinalLiftIO :: MonadIO m
               => Sem [Final m, Lift m, Lift IO] a
               -> m a
runFinalLiftIO = runFinalLift liftIO
