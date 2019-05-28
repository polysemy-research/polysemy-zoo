{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Polysemy.MTL where


import Polysemy
import Polysemy.State
import Data.Constraint
import Data.Constraint.Unsafe
import Data.Proxy
import Data.Reflection
import Data.Semigroup
import qualified Control.Monad.State.Class as S

newtype Lift1 (p :: (* -> *) -> Constraint) (m :: * -> *) (s) x = Lift1
  { lower1 :: m x
  } deriving (Functor, Applicative, Monad) via m

class ReifiableConstraint1 p where
  data Def1 (p :: (* -> *) -> Constraint) (m :: * -> *)
  reifiedIns1 :: Monad m => Reifies s (Def1 p m) :- p (Lift1 p m s)

-- > using (Monoid (+) 0) $ mappend mempty 12
-- > 12
using :: forall p m a. (Monad m, ReifiableConstraint1 p) => Def1 p m -> (p m => m a) -> m a
using d m =
  reify d $ \(_ :: Proxy s) -> m \\ trans (unsafeCoerceConstraint :: (p (Lift1 p m s) :- p m)) reifiedIns1

absorbState :: Member (State s) r => (forall m. S.MonadState s m => m a) -> Sem r a
absorbState f = using (MonadState get put) f

-- usingT :: forall p f a. ReifiableConstraint p => Def p a -> (p a => f a) -> f a
-- usingT d m = reify d $ \(_ :: Proxy s) -> m \\ trans (unsafeCoerceConstraint :: (p (Lift p a s) :- p a)) reifiedIns

instance ReifiableConstraint1 (S.MonadState s) where
  data Def1 (S.MonadState s) m = MonadState { get_ :: m s, put_ :: s -> m () }
  reifiedIns1 = Sub Dict

instance ( Monad m
         , Reifies s' (Def1 (S.MonadState s) m)
         ) => S.MonadState s (Lift1 (S.MonadState s) m s') where
  get = Lift1 $ get_ $ reflect $ Proxy @s'
  put s = Lift1 $ put_ (reflect $ Proxy @s') s
