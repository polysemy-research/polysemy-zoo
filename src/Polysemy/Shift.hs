{-# LANGUAGE Trustworthy #-}
module Polysemy.Shift
  (
    module Polysemy.Cont
    -- * Effect
  , Shift(..)

    -- * Actions
  , trap
  , invoke
  , abort
  , reset
  , reset'
  , shift

    -- * Interpretations
  , runShiftPure
  , runShiftM
  , shiftToFinal
  , runShiftWithCPure
  , runShiftWithCM

  , runContShiftPure
  , runContShiftM
  , runContShiftWithCPure
  , runContShiftWithCM

    -- * Unsafe Interpretations
  , runShiftUnsafe
  , runShiftWithCUnsafe
  , runContShiftUnsafe
  , runContShiftWithCUnsafe
  ) where


import Polysemy
import Polysemy.Cont
import Polysemy.Cont.Internal
import Polysemy.Shift.Internal
import Polysemy.Final
import Control.Monad.Cont (ContT(..))

import Polysemy.Internal
import Polysemy.Internal.Union


-----------------------------------------------------------------------------
-- | A variant of 'callCC'.
-- Executing the provided continuation will not abort execution.
--
-- Any effectful state of effects which have been run before the interpreter for
-- 'Shift' will be embedded in the return value of the continuation,
-- and therefore the continuation won't have any apparent effects unless these
-- effects are interpreted in the final monad.
--
-- Any higher-order actions will also not interact with the continuation in any
-- meaningful way; i.e. 'Polysemy.Reader.local' or 'Polysemy.Writer.censor' does
-- not affect it, 'Polysemy.Error.catch' will fail to catch any of its exceptions,
-- and 'Polysemy.Writer.listen' will always return 'mempty'.
--
-- The provided continuation may fail locally in its subcontinuations.
-- It may sometimes become necessary to handle such cases, in
-- which case such failure may be detected by using 'reset\'' together
-- with the provided continuation.
shift :: Member (Shift ref s) r
      => ((a -> Sem r s) -> Sem r s)
      -> Sem r a
shift cc = trap $ \ref -> cc (invoke ref)
{-# INLINE shift #-}

-----------------------------------------------------------------------------
-- | Runs a 'Shift' effect by providing @'pure' '.' 'Just'@ as the final
-- continuation.
--
-- The final return type is wrapped in a 'Maybe' due to the fact that
-- any continuation may fail locally.
--
-- This is a safe variant of 'runShiftUnsafe', as this may only be used
-- as the final interpreter before 'run'.
runShiftPure :: Sem '[Shift (Ref (Sem '[]) (Maybe a)) a] a
             -> Sem '[] (Maybe a)
runShiftPure = runShiftUnsafe
{-# INLINE runShiftPure #-}

-----------------------------------------------------------------------------
-- | Runs a 'Shift' effect by providing @'pure' '.' 'Just'@ as the final
-- continuation.
--
-- The final return type is wrapped in a 'Maybe' due to the fact that
-- any continuation may fail locally.
--
-- This is a safe variant of 'runShiftUnsafe', as this may only be used
-- as the final interpreter before 'runM'.
runShiftM :: Sem '[Shift (Ref (Sem '[Embed m]) (Maybe a)) a, Embed m] a
          -> Sem '[Embed m] (Maybe a)
runShiftM = runShiftUnsafe
{-# INLINE runShiftM #-}

-----------------------------------------------------------------------------
-- | Runs a 'Shift' effect in terms of a final 'ContT'
--
-- /Beware/: Effects that aren't interpreted in terms of the final monad
-- will have local state semantics in regards to 'Shift' effects
-- interpreted this way. See 'Final'.
shiftToFinal :: forall s m a r
              .  (Member (Final (ContT (Maybe s) m)) r, Monad m)
              => Sem (Shift (Ref m (Maybe s)) s ': r) a
              -> Sem r a
shiftToFinal = interpretFinal $ \case
  Trap main -> do
    main'         <- bindS main
    s             <- getInitialStateS
    Inspector ins <- getInspectorS
    pure $ ContT $ \c ->
      runContT (main' (Ref (c . (<$ s)) <$ s)) (pure . ins)
  Invoke ref a -> liftS $ ContT $ \c -> runRef ref a >>= maybe (pure Nothing) c
  Abort s -> pure $ ContT $ \_ -> pure (Just s)
  Reset main -> do
    main'         <- runS main
    Inspector ins <- getInspectorS
    liftS $ ContT $ \c ->
      runContT main' (pure . ins) >>= maybe (pure Nothing) c
  Reset' main -> do
    main'         <- runS main
    Inspector ins <- getInspectorS
    liftS $ ContT $ \c ->
      runContT main' (pure . ins) >>= c
{-# INLINE shiftToFinal #-}

-----------------------------------------------------------------------------
-- | Runs a 'Shift' effect by explicitly providing a final
-- continuation.
--
-- The final return type is wrapped in a 'Maybe' due to the fact that
-- any continuation may fail locally.
--
-- This is a safe variant of 'runShiftWithCUnsafe', as this may only be used
-- as the final interpreter before 'run'.
runShiftWithCPure :: (a -> Sem '[] (Maybe b))
                  -> Sem '[Shift (Ref (Sem '[]) (Maybe b)) b] a
                  -> Sem '[] (Maybe b)
runShiftWithCPure = runShiftWithCUnsafe
{-# INLINE runShiftWithCPure #-}

-----------------------------------------------------------------------------
-- | Runs a 'Shift' effect by explicitly providing a final
-- continuation.
--
-- The final return type is wrapped in a 'Maybe' due to the fact that
-- any continuation may fail locally.
--
-- This is a safe variant of 'runShiftWithCUnsafe', as this may only be used
-- as the final interpreter before 'runM'.
runShiftWithCM :: (a -> Sem '[Embed m] (Maybe b))
               -> Sem '[Shift (Ref (Sem '[Embed m]) (Maybe b)) b, Embed m] a
               -> Sem '[Embed m] (Maybe b)
runShiftWithCM = runShiftWithCUnsafe
{-# INLINE runShiftWithCM #-}

-----------------------------------------------------------------------------
-- | Runs a 'Cont' and a 'Shift' effect simultaneously by providing
-- @'pure' '.' 'Just'@ as the final continuation.
--
-- The final return type is wrapped in a 'Maybe' due to the fact that
-- any continuation may fail locally.
--
-- This is a safe variant of 'runContShiftUnsafe', as this may only be used
-- as the final interpreter before 'run'.
runContShiftPure :: Sem [ Cont (Ref (Sem '[]) (Maybe a))
                        , Shift (Ref (Sem '[]) (Maybe a)) a
                        ] a
                 -> Sem '[] (Maybe a)
runContShiftPure = runContShiftUnsafe
{-# INLINE runContShiftPure #-}

-----------------------------------------------------------------------------
-- | Runs a 'Cont' and a 'Shift' effect simultaneously by providing
-- @'pure' '.' 'Just'@ as the final continuation.
--
-- The final return type is wrapped in a 'Maybe' due to the fact that
-- any continuation may fail locally.
--
-- This is a safe variant of 'runContShiftUnsafe', as this may only be used
-- as the final interpreter before 'runM'.
runContShiftM :: Sem [ Cont (Ref (Sem '[Embed m]) (Maybe a))
                     , Shift (Ref (Sem '[Embed m]) (Maybe a)) a
                     , Embed m
                     ] a
              -> Sem '[Embed m] (Maybe a)
runContShiftM = runContShiftUnsafe
{-# INLINE runContShiftM #-}

-----------------------------------------------------------------------------
-- | Runs a 'Cont' and a 'Shift' effect simultaneously by explicitly providing
-- a final continuation.
--
-- The final return type is wrapped in a 'Maybe' due to the fact that
-- any continuation may fail locally.
--
-- This is a safe variant of 'runContShiftWithCUnsafe', as this may only be
-- used as the final interpreter before 'run'.
runContShiftWithCPure :: (a -> Sem '[] (Maybe s))
                      -> Sem [ Cont (Ref (Sem '[]) (Maybe s))
                             , Shift (Ref (Sem '[]) (Maybe s)) s
                             ] a
                      -> Sem '[] (Maybe s)
runContShiftWithCPure = runContShiftWithCUnsafe
{-# INLINE runContShiftWithCPure #-}

-----------------------------------------------------------------------------
-- | Runs a 'Cont' and a 'Shift' effect simultaneously by explicitly providing
-- a final continuation.
--
-- The final return type is wrapped in a 'Maybe' due to the fact that
-- any continuation may fail locally.
--
-- This is a safe variant of 'runContShiftWithCUnsafe', as this may only be used
-- as the final interpreter before 'runM'.
runContShiftWithCM :: (a -> Sem '[Embed m] (Maybe s))
                   -> Sem [ Cont (Ref (Sem '[Embed m]) (Maybe s))
                          , Shift (Ref (Sem '[Embed m]) (Maybe s)) s
                          , Embed m
                          ] a
                   -> Sem '[Embed m] (Maybe s)
runContShiftWithCM = runContShiftWithCUnsafe
{-# INLINE runContShiftWithCM #-}


-----------------------------------------------------------------------------
-- | Runs a 'Shift' effect by providing @'pure' '.' 'Just'@
-- as the final continuation.
--
-- The final return type is wrapped in a 'Maybe' due to the fact that
-- any continuation may fail locally.
--
-- __Beware__: This interpreter will invalidate all higher-order effects of any
-- interpreter run after it; i.e. 'Polysemy.Reader.local' and
-- 'Polysemy.Writer.censor' will be no-ops, 'Polysemy.Error.catch' will fail
-- to catch exceptions, and 'Polysemy.Writer.listen' will always return 'mempty'.
--
-- __You should therefore use 'runShiftUnsafe' /after/ running all__
-- __interpreters for your higher-order effects.__
runShiftUnsafe :: Sem (Shift (Ref (Sem r) (Maybe a)) a ': r) a -> Sem r (Maybe a)
runShiftUnsafe = runShiftWithCUnsafe (pure . Just)
{-# INLINE runShiftUnsafe #-}

-----------------------------------------------------------------------------
-- | Runs a 'Shift' effect by explicitly providing a final continuation.
--
-- The final return type is wrapped in a 'Maybe' due to the fact that any
-- continuation may fail locally.
--
-- __Beware__: This interpreter will invalidate all higher-order effects of any
-- interpreter run after it; i.e. 'Polysemy.Reader.local' and
-- 'Polysemy.Writer.censor' will be no-ops, 'Polysemy.Error.catch' will fail
-- to catch exceptions, and 'Polysemy.Writer.listen' will always return 'mempty'.
--
-- __You should therefore use 'runShiftWithCUnsafe' /after/ running all__
-- __interpreters for your higher-order effects.__
runShiftWithCUnsafe :: forall s a r.
                       (a -> Sem r (Maybe s))
                    -> Sem (Shift (Ref (Sem r) (Maybe s)) s ': r) a
                    -> Sem r (Maybe s)
runShiftWithCUnsafe c (Sem sem) = (`runContT` c) $ sem $ \u -> case decomp u of
  Right weaving -> runShiftWeaving runShiftWithCUnsafe weaving
  Left g -> ContT $ \c' -> embedSem g >>= runShiftWithCUnsafe c'
{-# INLINE runShiftWithCUnsafe #-}

-----------------------------------------------------------------------------
-- | Runs a 'Cont' and a 'Shift' effect simultaneously by providing
-- @'pure' '.' 'Just'@ as the final continuation.
--
-- The final return type is wrapped in a 'Maybe' due to the fact that
-- any continuation may fail locally.
--
-- __Beware__: This interpreter will invalidate all higher-order effects of any
-- interpreter run after it; i.e. 'Polysemy.Reader.local' and
-- 'Polysemy.Writer.censor' will be no-ops, 'Polysemy.Error.catch' will fail
-- to catch exceptions, and 'Polysemy.Writer.listen' will always return 'mempty'.
--
-- __You should therefore use 'runContShiftUnsafe' /after/ running all__
-- __interpreters for your higher-order effects.__
runContShiftUnsafe :: Sem (   Cont (Ref (Sem r) (Maybe a))
                           ': Shift (Ref (Sem r) (Maybe a)) a
                           ': r) a
                   -> Sem r (Maybe a)
runContShiftUnsafe = runContShiftWithCUnsafe (pure . Just)
{-# INLINE runContShiftUnsafe #-}

-----------------------------------------------------------------------------
-- | Runs a 'Cont' and a 'Shift' effect simultaneously by explicitly providing
-- a final continuation.
--
-- The final return type is wrapped in a 'Maybe' due to the fact that
-- any continuation may fail locally.
--
-- __Beware__: This interpreter will invalidate all higher-order effects of any
-- interpreter run after it; i.e. 'Polysemy.Reader.local' and
-- 'Polysemy.Writer.censor' will be no-ops, 'Polysemy.Error.catch' will fail
-- to catch exceptions, and 'Polysemy.Writer.listen' will always return 'mempty'.
--
-- __You should therefore use 'runContShiftWithCUnsafe' /after/ running all__
-- __interpreters for your higher-order effects.__
runContShiftWithCUnsafe :: forall s a r.
                           (a -> Sem r (Maybe s))
                        -> Sem (   Cont (Ref (Sem r) (Maybe s))
                                ': Shift (Ref (Sem r) (Maybe s)) s
                                ': r) a
                        -> Sem r (Maybe s)
runContShiftWithCUnsafe c (Sem m) = (`runContT` c) $ m $ \u -> case decomp u of
  Right weaving -> runContWeaving runContShiftWithCUnsafe weaving
  Left g -> case decomp g of
    Right weaving -> runShiftWeaving runContShiftWithCUnsafe weaving
    Left g'       -> ContT $ \c' -> embedSem g' >>= runContShiftWithCUnsafe c'
{-# INLINE runContShiftWithCUnsafe #-}
