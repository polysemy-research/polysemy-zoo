{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE UndecidableInstances        #-}

module Polysemy.ConstraintAbsorber.MonadError
  ( absorbError
  ) where

import qualified Control.Monad.Error.Class as S
import           Polysemy
import           Polysemy.ConstraintAbsorber
import           Polysemy.Error


------------------------------------------------------------------------------
-- | Introduce a local 'S.MonadError' constraint on 'Sem' --- allowing it to
-- interop nicely with MTL.
--
-- @since 0.3.0.0
absorbError
    :: Member (Error e) r
    => (S.MonadError e (Sem r) => Sem r a)
       -- ^ A computation that requires an instance of 'S.MonadError' for
       -- 'Sem'. This might be something with type @'S.MonadError' e m => m a@.
    -> Sem r a
absorbError = absorbWithSem @(S.MonadError _) @Action
  (ErrorDict throw catch)
  (Sub Dict)
{-# INLINEABLE absorbError #-}


------------------------------------------------------------------------------
-- | A dictionary of the functions we need to supply
-- to make an instance of Error
data ErrorDict e m = ErrorDict
  { throwError_ :: forall a. e -> m a
  , catchError_ :: forall a. m a -> (e -> m a) -> m a
  }


------------------------------------------------------------------------------
-- | Wrapper for a monadic action with phantom
-- type parameter for reflection.
-- Locally defined so that the instance we are going
-- to build with reflection must be coherent, that is
-- there cannot be orphans.
newtype Action m s' a = Action { action :: m a }
  deriving (Functor, Applicative, Monad)


------------------------------------------------------------------------------
-- | Given a reifiable mtl Error dictionary,
-- we can make an instance of @MonadError@ for the action
-- wrapped in @Action@.
instance ( Monad m
         , Reifies s' (ErrorDict e m)
         ) => S.MonadError e (Action m s') where
  throwError e = Action $ throwError_ (reflect $ Proxy @s') e
  {-# INLINEABLE throwError #-}
  catchError x f = Action $ catchError_ (reflect $ Proxy @s') (action x) (action . f)
  {-# INLINEABLE catchError #-}
