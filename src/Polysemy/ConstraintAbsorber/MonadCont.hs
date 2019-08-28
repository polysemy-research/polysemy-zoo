{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE UndecidableInstances        #-}

module Polysemy.ConstraintAbsorber.MonadCont
  ( absorbCont
  ) where

import           Data.Coerce
import qualified Control.Monad.Cont.Class as C
import           Polysemy
import           Polysemy.ConstraintAbsorber
import           Polysemy.Cont


------------------------------------------------------------------------------
-- | Introduce a local 'C.MonadCont' constraint on 'Sem' --- allowing it to
-- interop nicely with MTL.
--
-- @since 0.3.0.0
absorbCont
    :: Member (Cont ref) r
    => (C.MonadCont (Sem r) => Sem r a)
       -- ^ A computation that requires an instance of 'C.MonadCont' for
       -- 'Sem'. This might be something with type @'C.MonadCont' m => m a@.
    -> Sem r a
absorbCont = absorbWithSem @C.MonadCont @Action
  (ContDict callCC)
  (Sub Dict)
{-# INLINEABLE absorbCont #-}


------------------------------------------------------------------------------
-- | A dictionary of the functions we need to supply
-- to make an instance of Cont
newtype ContDict m = ContDict
  { callCC_ :: forall a b. ((a -> m b) -> m a) -> m a
  }


------------------------------------------------------------------------------
-- | Wrapper for a monadic action with phantom
-- type parameter for reflection.
-- Locally defined so that the instance we are going
-- to build with reflection must be coherent, that is
-- there cannot be orphans.
newtype Action m s' a = Action (m a)
  deriving (Functor, Applicative, Monad)


------------------------------------------------------------------------------
-- | Given a reifiable mtl Cont dictionary,
-- we can make an instance of @MonadCont@ for the action
-- wrapped in @Action@.
instance ( Monad m
         , Reifies s' (ContDict m)
         ) => C.MonadCont (Action m s') where
  callCC (cc :: (a -> Action m s' b) -> Action m s' a)
    = Action $ callCC_ (reflect $ Proxy @s') @a @b (coerce cc)
  {-# INLINEABLE callCC #-}
