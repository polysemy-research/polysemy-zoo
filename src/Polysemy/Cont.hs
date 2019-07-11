module Polysemy.Cont
  (-- * Effect
    Cont(..)

    -- * Actions
  , jump
  , subst
  , callCC

    -- * Interpretations
  , runContPure
  , runContM
  , runContFinal

    -- * Unsafe Interpretations
  , runContUnsafe

  -- * Prompt types
  , Ref(..)
  , ExitRef(..)
  ) where

import Data.Void

import Polysemy
import Polysemy.Final

import Polysemy.Cont.Internal

import Control.Monad.Cont (MonadCont())
import qualified Control.Monad.Cont as C (callCC)

-----------------------------------------------------------------------------
-- | Call with current continuation.
-- Executing the provided continuation will abort execution.
--
-- Using the provided continuation
-- will rollback all effectful state back to the point where 'callCC' was invoked,
-- unless such state is interpreted in terms of the final
-- monad, /or/ the associated interpreter of the effectful state
-- is run after 'runContUnsafe', which may be done if the effect isn't
-- higher-order.
--
-- Higher-order effects do not interact with the continuation in any meaningful
-- way; i.e. 'Polysemy.Reader.local' or 'Polysemy.Writer.censor' does not affect
-- it, and 'Polysemy.Error.catch' will fail to catch any of its exceptions.
-- The only exception to this is if you interpret such effects /and/ 'Cont'
-- in terms of the final monad, and the final monad can perform such interactions
-- in a meaningful manner.
callCC :: forall ref a r.
          Member (Cont ref) r
       => ((forall b. a -> Sem r b) -> Sem r a)
       -> Sem r a
callCC cc = subst (\ref -> cc (jump ref)) pure
{-# INLINE callCC #-}

-----------------------------------------------------------------------------
-- | Runs a 'Cont' effect by providing 'pure' as the final continuation.
--
-- This is a safe variant of 'runContUnsafe', as this may only be used
-- as the final interpreter before 'run'.
runContPure :: Sem '[Cont (Ref (Sem '[]) a)] a -> Sem '[] a
runContPure = runContUnsafe
{-# INLINE runContPure #-}

-----------------------------------------------------------------------------
-- | Runs a 'Cont' effect by providing 'pure' as the final continuation.
--
-- This is a safe variant of 'runContUnsafe', as this may only be used
-- as the final interpreter before 'runM'.
runContM :: Sem '[Cont (Ref (Sem '[Lift m]) a), Lift m] a -> Sem '[Lift m] a
runContM = runContUnsafe
{-# INLINE runContM #-}

-----------------------------------------------------------------------------
-- | Runs a 'Cont' effect in terms of a final 'MonadCont'
--
-- /Beware/: Effects that aren't interpreted in terms of the final monad
-- will have local state semantics in regards to 'Cont' effects
-- interpreted this way. See 'interpretFinal'.
runContFinal :: (Member (Final m) r, MonadCont m)
             => Sem (Cont (ExitRef m) ': r) a
             -> Sem r a
runContFinal = interpretFinal $ \case
  Jump ref a    -> pure $ enterExit ref a
  Subst main cb -> do
    main' <- bindS main
    cb'   <- bindS cb
    s     <- getInitialStateS
    pure $ C.callCC $ \exit ->
      main' (ExitRef (\a -> cb' (a <$ s) >>= vacuous . exit) <$ s)
{-# INLINE runContFinal #-}

-----------------------------------------------------------------------------
-- | Runs a 'Cont' effect by providing 'pure' as the final continuation.
--
-- __Beware__: This interpreter will invalidate all higher-order effects of any
-- interpreter run after it; i.e. 'Polysemy.Reader.local' and
-- 'Polysemy.Writer.censor' will be no-ops, 'Polysemy.Error.catch' will fail
-- to catch exceptions, and 'Polysemy.Writer.listen' will always return 'mempty'.
--
-- __You should therefore use 'runContUnsafe' /after/ running all interpreters for
-- your higher-order effects.__
--
-- Note that 'Final' is a higher-order effect, and thus 'runContUnsafe' can't
-- safely be used together with 'runFinal'.
runContUnsafe :: Sem (Cont (Ref (Sem r) a) ': r) a -> Sem r a
runContUnsafe = runContWithCUnsafe pure
{-# INLINE runContUnsafe #-}
