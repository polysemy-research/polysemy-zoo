{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE UndecidableInstances        #-}

module Polysemy.ConstraintAbsorber.MonadState
  ( absorbState
  ) where

import           Polysemy
import           Polysemy.ConstraintAbsorber
import           Polysemy.State
import qualified Control.Monad.State.Class as S


------------------------------------------------------------------------------
-- | Introduce a local 'S.MonadState' constraint on 'Sem' --- allowing it to
-- interop nicely with MTL.
--
-- @since 0.3.0.0
absorbState
    :: Member (State s) r
    => (S.MonadState s (Sem r) => Sem r a)
       -- ^ A computation that requires an instance of 'S.MonadState' for
       -- 'Sem'. This might be something with type @'S.MonadState' s m => m a@.
    -> Sem r a
absorbState = absorbWithSem @(S.MonadState _) @Action
  (StateDict get put)
  (Sub Dict)
{-# INLINEABLE absorbState #-}


------------------------------------------------------------------------------
-- | A Dictionary of the functions we need to supply
-- to make an instance of State
data StateDict s m = StateDict
  { get_ :: m s
  , put_ :: s -> m ()
  }


------------------------------------------------------------------------------
-- | Wrapper for a monadic action with phantom type parameter for reflection.

-- Locally defined so that the instance we are going to build with reflection
-- must be coherent, that is there cannot be orphans.
newtype Action m s' a = Action (m a)
  deriving (Functor, Applicative, Monad)


------------------------------------------------------------------------------
-- | Given a reifiable mtl State dictionary,
-- we can make an instance of @MonadState@ for the action
-- wrapped in @Action@.
instance ( Monad m
         , Reifies s' (StateDict s m)
         ) => S.MonadState s (Action m s') where
  get = Action $ get_ $ reflect $ Proxy @s'
  {-# INLINEABLE get #-}
  put s = Action $ put_ (reflect $ Proxy @s') s
  {-# INLINEABLE put #-}

