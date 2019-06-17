{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE UndecidableInstances        #-}

module Polysemy.ConstraintAbsorber.MonadReader
  ( absorbReader
  ) where

import qualified Control.Monad.Reader.Class as S
import           Polysemy
import           Polysemy.ConstraintAbsorber
import           Polysemy.Reader


------------------------------------------------------------------------------
-- | Introduce a local 'S.MonadReader' constraint on 'Sem' --- allowing it to
-- interop nicely with MTL.
--
-- @since 0.3.0.0
absorbReader
    :: Member (Reader i) r
    => (S.MonadReader i (Sem r) => Sem r a)
       -- ^ A computation that requires an instance of 'S.MonadReader' for
       -- 'Sem'. This might be something with type @'S.MonadReader' r m => m a@.
    -> Sem r a
absorbReader = absorbWithSem @(S.MonadReader _) @Action
  (ReaderDict ask local)
  (Sub Dict)
{-# INLINEABLE absorbReader #-}


------------------------------------------------------------------------------
-- | A dictionary of the functions we need to supply
-- to make an instance of Reader
data ReaderDict i m = ReaderDict
  { ask_ :: m i
  , local_ :: forall a. (i -> i) -> m a -> m a
  }


------------------------------------------------------------------------------
-- | Wrapper for a monadic action with phantom
-- type parameter for reflection.
-- Locally defined so that the instance we are going
-- to build with reflection must be coherent, that is
-- there cannot be orphans.
newtype Action m s a = Action { action :: m a }
  deriving (Functor, Applicative, Monad)


------------------------------------------------------------------------------
-- | Given a reifiable mtl Reader dictionary,
-- we can make an instance of @MonadReader@ for the action
-- wrapped in @Action@.
instance ( Monad m
         , Reifies s' (ReaderDict i m)
         ) => S.MonadReader i (Action m s') where
  ask = Action $ ask_ $ reflect $ Proxy @s'
  {-# INLINEABLE ask #-}
  local f m = Action $ local_ (reflect $ Proxy @s') f $ action m
  {-# INLINEABLE local #-}

