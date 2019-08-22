{-# LANGUAGE TemplateHaskell, Unsafe #-}
module Polysemy.Cont.Internal where

import Data.Functor.Contravariant

import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Union
import Polysemy.Fresh
import Polysemy.Error

import Control.Monad
import Control.Monad.Trans.Cont hiding (Cont)

import Unsafe.Coerce
import GHC.Exts (Any)

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
jump :: forall ref a b r.
        Member (Cont ref) r
     => ref a
     -> a
     -> Sem r b

-----------------------------------------------------------------------------
-- | Reifies the current continuation in the form of a prompt, and passes it to
-- the first argument. If the prompt becomes invoked via 'jump', then the
-- second argument will be run before the reified continuation, and otherwise
-- will not be called at all.
subst :: forall ref a b r
      .  Member (Cont ref) r
      => (ref a -> Sem r b)
      -> (a -> Sem r b)
      -> Sem r b

-----------------------------------------------------------------------------
-- | Runs a 'Cont' effect by providing a final continuation.
--
-- __Beware__: This interpreter will invalidate all higher-order effects of any
-- interpreter run after it; i.e. 'Polysemy.Reader.local' and
-- 'Polysemy.Writer.censor' will be no-ops, 'Polysemy.Error.catch' will fail
-- to catch exceptions, and 'Polysemy.Writer.listen' will always return 'mempty'.
--
-- __You should therefore use 'runContWithCUnsafe' /after/ running all interpreters
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

instance Contravariant (Ref m s) where
  contramap f ref = Ref (runRef ref . f)

newtype ExitRef m a = ExitRef { enterExit :: forall b. a -> m b }

instance Contravariant (ExitRef m) where
  contramap f ref = ExitRef $ \a -> enterExit ref (f a)

data ViaFreshRef uniq a = ViaFreshRef { getBacktrackException :: a -> (uniq, Any) }

instance Contravariant (ViaFreshRef uniq) where
  contramap f ref = ViaFreshRef $ \a -> getBacktrackException ref (f a)

{-
  KingoftheHomeless: OK, so let's discuss how this works.
  The idea here is to instead of providing a monadic computation
  to the call of 'callCC' that simply short-circuits everything like
  'ContT' does, we fake that behaviour by instead providing an
  exception to 'callCC', and then try to 'catch' that exception
  on the continuation. If the exception is caught, then we run the
  continuation again. This way, we can get abortive continuations
  without having to scope over a result type variable, avoiding
  the problem that 'runContUnsafe' has, and making it possible
  to weave other effects through without breaking everything.

  Even with that solution, weaving effects through have more problems
  of their own; namely, if we simply lower a
  'forall s. ContT s (Sem r) a' to 'Sem r a', then we effectively
  delimit all higher-order computations. This is bad, because
  if a reified continuation produced within
  the higher-order computation escapes from it,
  then nothing can catch the underlying backtrack exception
  once it is thrown.

  The solution to this is anothor kludge: when weaving other effects through,
  we instead use 'runContViaFreshInCWeave'; this makes use 'ContFreshState'
  as its functorial state, which stores /handlers/ for backtrack exceptions.
  'runContViaFreshInCWeave', in addition to 'catch'ing exceptions on the continuation
  it is given, /also/ returns the handler it uses for the 'catch'.
  This handler is then used by 'runContViaFresh' to catch exceptions on the
  continuation /it/ gets, but can't provide to the higher-order computation.

  I'm astonished that this even remotely works, but it does have some rather
  weird behaviour I haven't completely figured out yet.

  I'm reasonably happy with how 'runContViaFreshInC' looks;
  I'm a lot less happy with 'runContViaFreshInCWeave', I just kinda threw
  it haphazardly. I figure most weirdness stem from issues in
  'runContViaFreshInCWeave', so I need to think it through some more.
-}
-- | Intermediary monadic interpretation used for running 'runContViaFresh'.
-- See source for a discussion on how this works.
runContViaFreshInC :: forall uniq s r a
                    . (Member (Fresh uniq) r, Eq uniq)
                   => Sem (Cont (ViaFreshRef uniq) ': r) a
                   -> ContT s (Sem (Error (uniq, Any) ': r)) a
runContViaFreshInC = usingSem $ \u -> ContT $ \c ->
  case decomp u of
    Right (Weaving e s wv ex _) ->
      case e of
        Subst main cn -> do
          ref <- fresh
          let
            main' = runContViaFreshInC . wv . fmap main . (<$ s)
            cn'   = runContViaFreshInC . wv . fmap cn . (<$ s)
            loop act =
              runContT (ex <$> act) c `catch` \ x@(ref', a') -> do
                if ref == ref' then
                  loop (cn' $ unsafeCoerce a')
                else
                  throw x
          loop $ main' $ ViaFreshRef (\a -> (ref, unsafeCoerce a))
        Jump ref a -> throw (getBacktrackException ref a)
    Left g -> do
      ResAndHandler a rc <- liftSem $
        weave
          (ResAndHandler @uniq @r () throw)
          -- TODO(KingoftheHomeless): is this the distributive law we want?
          (\(ResAndHandler a rc) ->
            runContT
              (runContViaFreshInCWeave a)
              (\x -> pure $
                ResAndHandler
                  x
                  (rc >=> (`runContT` pure) . runContViaFreshInC)
              )
          )
          (Just . getResult)
          (weaken g)
      let loop x = c x `catch` (rc >=> loop)
      loop a

-- | A variant of 'runContViaFreshInC' which it uses when weaving other effects through.
runContViaFreshInCWeave :: forall uniq s r a
                         . (Member (Fresh uniq) r, Eq uniq)
                        => Sem (Cont (ViaFreshRef uniq) ': r) a
                        -> ContT (ContFreshState uniq r s)
                            (Sem (Error (uniq, Any) ': r))
                            a
runContViaFreshInCWeave = usingSem $ \u -> ContT $ \c ->
  case decomp u of
    Right (Weaving e s wv ex _) ->
      case e of
        Subst main cn -> do
          ref <- fresh
          let
            -- TODO(KingoftheHomeless): runContViaFreshInC?
            main' = runContViaFreshInCWeave . wv . fmap main . (<$ s)
            cn'   = runContViaFreshInCWeave . wv . fmap cn . (<$ s)
            loop act =
              runContT (ex <$> act) c `catch` \ x@(ref', a') -> do
                if ref == ref' then
                  loop (cn' $ unsafeCoerce a')
                else
                  throw x
          ResAndHandler res h <-
            loop $ main' $ ViaFreshRef (\a -> (ref, unsafeCoerce a))
          return $ ResAndHandler res
              -- TODO(KingoftheHomeless): This handler is dubious.
            $ \x -> fmap getResult $ loop $ ContT $ \_ -> fmap (`ResAndHandler` h) (h x)
        Jump ref a -> throw (getBacktrackException ref a)
    Left g -> do
      ResAndHandler a h <- liftSem $
        weave
          (ResAndHandler @uniq @r () throw)
          (\(ResAndHandler a rc) ->
            runContT
              (runContViaFreshInCWeave a)
              (\x -> pure $
                ResAndHandler
                  x
                  (rc >=> (`runContT` pure) . runContViaFreshInC)
              )
          )
          (Just . getResult)
          (weaken g)
      let loop x = c x `catch` (h >=> loop)
      ResAndHandler res h' <- loop a
      -- TODO(KingoftheHomeless): This handler is dubious.
      return (ResAndHandler res $ \x -> (h' x `catch` (h >=> fmap getResult . loop)))

-- | This is the effectful state used by 'runContViaFreshInC' when weaving through
-- other effectful actions. The point of it is to avoid delimiting computations
-- in higher-order effects, by having them return a handler which may be used
-- to intercept backtrack exceptions of the current continuation.
data ContFreshState uniq r a = ResAndHandler {
    getResult :: a
  , getHandler :: (uniq, Any) -> Sem (Error (uniq, Any) ': r) a
  }
  deriving Functor
