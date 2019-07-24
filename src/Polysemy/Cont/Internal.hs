{-# LANGUAGE TemplateHaskell #-}
module Polysemy.Cont.Internal where

import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Union
import Control.Monad
import Control.Monad.Cont (ContT(..))

-----------------------------------------------------------------------------
-- | An effect for abortive continuations.
--
-- Formulated à la Tom Schrijvers et al.
-- "Monad Transformers and Modular Algebraic Effects: What Binds Them Together"
-- (2016). <http://www.cs.kuleuven.be/publicaties/rapporten/cw/CW699.pdf>
--
-- Activating polysemy-plugin is highly recommended when using this effect
-- in order to avoid ambiguous types.
data Cont ref m a where
  Jump    :: ref a -> a -> Cont ref m b
  Subst   :: (ref a -> m b) -> (a -> m b) -> Cont ref m b

makeSem_ ''Cont

-----------------------------------------------------------------------------
-- | Provide an answer to a prompt, jumping to its reified continuation,
-- and aborting the current continuation.
--
-- Using 'jump' will rollback all effectful state back to the point where the
-- prompt was created, unless such state is interpreted in terms of the final
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
jump :: forall ref x a r.
        Member (Cont ref) r
     => ref x
     -> x
     -> Sem r a

-----------------------------------------------------------------------------
-- | Reifies the current continuation in the form of a prompt, and passes it to
-- the first argument. If the prompt becomes invoked via 'jump', then the
-- second argument will be run before the reified continuation, and otherwise
-- will not be called at all.
subst :: forall ref x a r.
         Member (Cont ref) r
      => (ref x -> Sem r a)
      -> (x -> Sem r a)
      -> Sem r a

-----------------------------------------------------------------------------
-- | Runs a 'Cont' effect by providing a final continuation.
--
-- __Beware__: This interpreter will invalidate all higher-order effects of any
-- interpreter run after it; i.e. 'Polysemy.Reader.local' and
-- 'Polysemy.Writer.censor' will be no-ops, 'Polysemy.Error.catch' will fail
-- to catch exceptions, and 'Polysemy.Writer.listen' will always return 'mempty'.
--
-- __You should therefore use 'runContUnsafeWithC' /after/ running all interpreters
-- for your higher-order effects.__
runContWithCUnsafe :: (a -> Sem r s) -> Sem (Cont (Ref (Sem r) s) ': r) a -> Sem r s
runContWithCUnsafe c (Sem m) = (`runContT` c) $ m $ \u -> case decomp u of
  Right weaving -> runContWeaving runContWithCUnsafe weaving
  Left g -> ContT $ \c' -> embedSem g >>= runContWithCUnsafe c'
{-# INLINE runContWithCUnsafe #-}

runContWeaving :: Monad m
               => (forall x. (x -> m s) -> Sem r x -> m s)
               -> Weaving (Cont (Ref m s)) (Sem r) a
               -> ContT s m a
runContWeaving runW (Weaving e s wv ex _) =
    ContT $ \c ->
      case e of
        Jump ref a    -> runRef ref a
        Subst main cb ->
          let
            callback a = runW (c . ex) (wv (cb a <$ s))
          in
            runW (c . ex) (wv (main (Ref callback) <$ s))
{-# INLINE runContWeaving #-}

inspectSem :: Sem r a -> Maybe a
inspectSem (Sem m) = m (\_ -> Nothing)
{-# INLINE inspectSem #-}

embedSem :: Union r (Sem r') a -> Sem r (Sem r' a)
embedSem = liftSem . weave (pure ()) (pure . join) inspectSem
{-# INLINE embedSem #-}

newtype Ref m s a = Ref { runRef :: a -> m s }
newtype ExitRef m a = ExitRef { enterExit :: forall b. a -> m b }
