{-# LANGUAGE Trustworthy #-}
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
  , contToFinal

    -- * Experimental Interpretations
  , runContViaFresh

    -- * Unsafe Interpretations
  , runContUnsafe

  -- * Prompt types
  , Ref(..)
  , ExitRef(..)
  , ViaFreshRef
  ) where

import Data.Void

import Polysemy
import Polysemy.Final

import Polysemy.Cont.Internal

import Polysemy.Error
import Polysemy.Fresh

import Control.Monad.Cont (MonadCont(), ContT(..), runContT)
import qualified Control.Monad.Cont as C (callCC)

-----------------------------------------------------------------------------
-- | Call with current continuation.
-- Executing the provided continuation will abort execution.
--
-- Using the provided continuation
-- will rollback all local effectful state back to the point where
-- 'callCC' was invoked.
--
-- Higher-order effects do not interact with the continuation in any meaningful
-- way; i.e. 'Polysemy.Reader.local' or 'Polysemy.Writer.censor' does not affect
-- it, and 'Polysemy.Error.catch' will fail to catch any of its exceptions.
-- The only exception to this is if you interpret such effects /and/ 'Cont'
-- in terms of the final monad, and the final monad can perform such interactions
-- in a meaningful manner.
callCC :: forall ref r a
       .  Member (Cont ref) r
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
runContM :: Sem '[Cont (Ref (Sem '[Embed m]) a), Embed m] a -> Sem '[Embed m] a
runContM = runContUnsafe
{-# INLINE runContM #-}

-----------------------------------------------------------------------------
-- | Runs a 'Cont' effect in terms of a final 'MonadCont'
--
-- /Beware/: Effects that aren't interpreted in terms of the final monad
-- will have local state semantics in regards to 'Cont' effects
-- interpreted this way. See 'Final'.
contToFinal :: (Member (Final m) r, MonadCont m)
            => Sem (Cont (ExitRef m) ': r) a
            -> Sem r a
contToFinal = interpretFinal $ \case
  Jump ref a    -> pure $ enterExit ref a
  Subst main cb -> do
    main' <- bindS main
    cb'   <- bindS cb
    s     <- getInitialStateS
    pure $ C.callCC $ \exit ->
      main' (ExitRef (\a -> cb' (a <$ s) >>= vacuous . exit) <$ s)
{-# INLINE contToFinal #-}

-----------------------------------------------------------------------------
-- | A highly experimental 'Cont' interpreter that functions
-- through a combination of 'Error' and 'Fresh'. This may be used safely
-- anywhere in the effect stack.
--
-- 'runContViaFresh' is still under development.
-- You're encouraged to experiment with it, but don't rely on it.
-- For best results, use 'runContViaFresh' as the first interpreter you run,
-- such that all other effects are global in respect to it.
--
-- This interpreter may return 'Nothing' if the control flow becomes
-- split into separate, inconsistent parts,
-- such that backtracking fails when trying to invoke continuations.
-- For example, if you reify a continuation inside an
-- 'async':ed thread, and then have that thread return the reified
-- continuation back to the main thread through an 'await', then
-- 'runContViaFresh' will return 'Nothing' upon executing the continuation
-- in the main thread.
runContViaFresh :: forall uniq r a
                 . (Member (Fresh uniq) r, Eq uniq)
                => Sem (Cont (ViaFreshRef uniq) ': r) a
                -> Sem r (Maybe a)
runContViaFresh =
  let
    hush (Right a) = Just a
    hush _         = Nothing
  in
      fmap hush
    . runError
    . (`runContT` pure)
    . runContViaFreshInC
{-# INLINE runContViaFresh #-}

-----------------------------------------------------------------------------
-- | Runs a 'Cont' effect by providing 'pure' as the final continuation.
--
-- __Beware__: This interpreter will invalidate all higher-order effects of any
-- interpreter run after it; i.e. 'Polysemy.Reader.local' and
-- 'Polysemy.Writer.censor' will be no-ops, 'Polysemy.Error.catch' will fail
-- to catch exceptions, and 'Polysemy.Writer.listen' will always return 'mempty'.
--
-- __You should therefore use 'runContUnsafe' only /after/ running all__
-- __interpreters for your higher-order effects.__
--
-- Note that 'Final' is a higher-order effect, and thus 'runContUnsafe' can't
-- safely be used together with 'runFinal'.
runContUnsafe :: Sem (Cont (Ref (Sem r) a) ': r) a -> Sem r a
runContUnsafe = runContWithCUnsafe pure
{-# INLINE runContUnsafe #-}
