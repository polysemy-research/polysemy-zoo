{-# LANGUAGE TemplateHaskell, Trustworthy #-}
module Polysemy.Capture
  (-- * Effect
    Capture(..)

    -- * Actions
  , reify
  , reflect
  , delimit
  , delimit'
  , capture

    -- * Interpretations
  , runCapture
  , runCaptureWithC

  -- * Prompt types
  , Ref(..)
  ) where

import Control.Monad
import Control.Monad.Cont (ContT(..))

import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Union

import Polysemy.Cont.Internal (Ref(..))

-----------------------------------------------------------------------------
-- | A less powerful variant of 'Polysemy.Shift.Shift' that may always be
-- interpreted safely. Unlike 'Polysemy.Shift.Shift',
-- continuations can't leave the scope in which they are provided.
--
-- __Note__: Any computation used in a higher-order effect will
-- be delimited.
--
-- Activating polysemy-plugin is highly recommended when using this effect
-- in order to avoid ambiguous types.
data Capture ref m a where
  Reify    :: (forall s. ref s a -> m s) -> Capture ref m a
  Reflect  :: ref s a -> a -> Capture ref m s
  Delimit  :: m a -> Capture ref m a
  Delimit' :: m a -> Capture ref m (Maybe a)

makeSem_ ''Capture

-----------------------------------------------------------------------------
-- | Reifies the current continuation in the form of a prompt, and passes it to
-- the first argument.
reify :: forall ref a r
      .  Member (Capture ref) r
      => (forall s. ref s a -> Sem r s)
      -> Sem r a

-----------------------------------------------------------------------------
-- | Provide an answer to a prompt, jumping to its reified continuation.
-- This will not abort the current continuation, and the
-- reified computation will return its final result when finished.
--
-- The provided continuation may fail locally in its subcontinuations.
-- It may sometimes become necessary to handle such cases. To do so,
-- use 'delimit'' together with 'reflect' (the reified continuation
-- is already delimited).
reflect :: forall ref s a r
        .  Member (Capture ref) r
        => ref s a
        -> a
        -> Sem r s

-----------------------------------------------------------------------------
-- | Delimits any continuations
delimit :: forall ref a r
        .  Member (Capture ref) r
        => Sem r a
        -> Sem r a

-----------------------------------------------------------------------------
-- | Delimits any continuations, and detects if any subcontinuation
-- has failed locally.
delimit' :: forall ref a r
         .  Member (Capture ref) r
         => Sem r a
         -> Sem r (Maybe a)

-----------------------------------------------------------------------------
-- | A restricted version of 'Polysemy.Shift.shift'.
-- Executing the provided continuation will not abort execution.
--
-- The provided continuation may fail locally in its subcontinuations.
-- It may sometimes become necessary to handle such cases, in
-- which case such failure may be detected by using 'delimit'' together
-- with the provided continuation (the provided continuation
-- is already delimited).
capture :: Member (Capture ref) r
        => (forall s. (a -> Sem r s) -> Sem r s)
        -> Sem r a
capture cc = reify (\ref -> cc (reflect ref))
{-# INLINE capture #-}

-----------------------------------------------------------------------------
-- | Runs a 'Capture' effect by providing @'pure' '.' 'Just'@ as the final
-- continuation.
--
-- The final return type is wrapped in a 'Maybe' due to the fact that
-- any continuation may fail locally.
runCapture :: Sem (Capture (Ref (Sem r))': r) a -> Sem r (Maybe a)
runCapture = runCaptureWithC (pure . Just)
{-# INLINE runCapture #-}

-----------------------------------------------------------------------------
-- | Runs a 'Capture' effect by explicitly providing a final
-- continuation.
--
-- The final return type is wrapped in a 'Maybe' due to the fact that
-- any continuation may fail locally.
runCaptureWithC :: (a -> Sem r (Maybe s))
                -> Sem (Capture (Ref (Sem r)) ': r) a
                -> Sem r (Maybe s)
runCaptureWithC c (Sem m) = (`runContT` c) $ m $ \u ->
    case decomp u of
      Right (Weaving e s wv ex ins) ->
        ContT $ \c' ->
          case e of
            Reflect ref a ->
                  runRef ref a
              >>= c' . ex . (<$ s)
            Reify main ->
              runCaptureWithC
                (pure . join . ins)
                (wv (main (Ref (c' . ex . (<$ s))) <$ s))
            Delimit main ->
                  runCaptureWithC
                    (pure . Just)
                    (wv (main <$ s))
              >>= maybe (pure Nothing) (c' . ex)
            Delimit' main ->
                  runCaptureWithC
                    (pure . Just)
                    (wv (main <$ s))
              >>= maybe (c' (ex (Nothing <$ s))) (c' . ex . fmap Just)
      Left g -> ContT $ \c' ->
            liftSem (weave (Just ()) (maybe (pure Nothing) runCapture) id g)
        >>= maybe (pure Nothing) c'
{-# INLINE runCaptureWithC #-}
