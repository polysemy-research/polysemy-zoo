{-# LANGUAGE TemplateHaskell #-}
module Polysemy.Shift.Internal where

import Polysemy
import Polysemy.Internal.Union
import Polysemy.Cont.Internal (Ref(..))
import Control.Monad.Cont (ContT(..))

-----------------------------------------------------------------------------
-- | An effect for delimited continuations, formulated algebraically
-- through a variant of the 'Polysemy.Cont.Jump/'Polysemy.Cont.Subst'
-- formulation of abortive continuations.
--
-- Activating polysemy-plugin is highly recommended when using this effect
-- in order to avoid ambiguous types.
data Shift ref s m a where
  Trap    :: (ref a -> m s) -> Shift ref s m a
  Invoke  :: ref a -> a -> Shift ref s m s
  Abort   :: s   -> Shift ref s m a
  Reset   :: m s -> Shift ref s m s
  Reset'  :: m s -> Shift ref s m (Maybe s)

makeSem_ ''Shift

-----------------------------------------------------------------------------
-- | Reifies the current continuation in the form of a prompt, and passes it to
-- the first argument. Unlike 'subst', control will never return to the current
-- continuation unless the prompt is invoked via 'release'.
trap :: forall ref s a r
     .  Member (Shift ref s) r
     => (ref a -> Sem r s)
     -> Sem r a

-----------------------------------------------------------------------------
-- | Provide an answer to a prompt, jumping to its reified continuation.
-- Unlike 'jump', this will not abort the current continuation, and the
-- reified computation will instead return its final result when finished.
--
-- Any effectful state of effects which have been run before the interpreter for
-- 'Shift' will be embedded in the return value, and therefore the invocation
-- won't have any apparent effects unless these are interpreted in the final
-- monad.
--
-- Any higher-order actions will also not interact with the continuation in any
-- meaningful way; i.e. 'Polysemy.Reader.local' or 'Polysemy.Writer.censor' does
-- not affect it, 'Polysemy.Error.catch' will fail to catch any of its exceptions,
-- and 'Polysemy.Writer.listen' will always return 'mempty'.
--
-- The provided continuation may fail locally in its subcontinuations.
-- It may sometimes become necessary to handle such cases. To do so,
-- use 'reset\'' together with 'release'.
invoke :: forall ref s a r
       .  Member (Shift ref s) r
       => ref a
       -> a
       -> Sem r s


-----------------------------------------------------------------------------
-- | Aborts the current continuation with a result.
abort :: forall ref s a r
      .  Member (Shift ref s) r
      => s
      -> Sem r a

-----------------------------------------------------------------------------
-- | Delimits any continuations and calls to 'abort'.
reset :: forall ref s r
      .  Member (Shift ref s) r
      => Sem r s
      -> Sem r s

-----------------------------------------------------------------------------
-- | Delimits any continuations and calls to 'abort', and detects if
-- any subcontinuation has failed locally.
reset' :: forall ref s r
       .  Member (Shift ref s) r
       => Sem r s
       -> Sem r (Maybe s)

runShiftWeaving :: Monad m
                => (forall x. (x -> m (Maybe s)) -> Sem r x -> m (Maybe s))
                -> Weaving (Shift (Ref m (Maybe s)) s) (Sem r) a
                -> ContT (Maybe s) m a
runShiftWeaving runW (Weaving e s wv ex ins) =
  fmap (ex . (<$ s)) $ ContT $ \c ->
    case e of
      Trap main ->
        runW (pure . ins) $ wv (main (Ref c) <$ s)
      Invoke ref a ->
        runRef ref a >>= maybe (pure Nothing) c
      Abort t -> pure (Just t)
      Reset main ->
        runW (pure . ins) (wv (main <$ s)) >>= maybe (pure Nothing) c
      Reset' main ->
        runW (pure . ins) (wv (main <$ s)) >>= c
{-# INLINE runShiftWeaving #-}
