{-# LANGUAGE AllowAmbiguousTypes         #-}
{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE UndecidableInstances        #-}
{-# LANGUAGE UndecidableSuperClasses     #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Polysemy.ConstraintAbsorber
  (
    -- * absorb-builders
    using
  , absorbWithSem
  
    -- * Re-exports
  , Reifies
  , (:-)(Sub)
  , Dict(Dict)
  , reflect
  , Proxy (Proxy)
  )
where

import           Polysemy

import qualified Data.Constraint as C
import           Data.Constraint (Dict(Dict),(:-)(Sub),(\\))
import qualified Data.Constraint.Unsafe as C
import           Data.Proxy (Proxy (..))
import qualified Data.Reflection as R
import           Data.Reflection (Reifies, reflect) 
import           Data.Kind (Type, Constraint)

------------------------------------------------------------------------------
-- | Given a reifiable constraint @p, and a dictionary, of type @d@, to use,
-- discharge the constraint by prvoding a dictionary, and a "reified instance"
-- (often @Sub Dict@ will do)
using :: forall (d :: Type)  -- ^ dictionary
                (x :: (Type -> Type) -> Type -> Type -> Type) -- ^ wrapper of constrained action
                (p :: (Type -> Type) -> Constraint) -- ^ Monadic constraint          
                (m :: Type -> Type) 
                (a :: Type) . Monad m
  => d
  -> (forall s. R.Reifies s d :- p (x m s))
  -> (p m => m a)
  -> m a
using d i m =
  R.reify d $ \(_ :: Proxy (s :: Type)) -> m \\ C.trans
  (C.unsafeCoerceConstraint :: ((p (x m s) :- p m))) i
{-# INLINEABLE using #-}
  
-- | Specialize using for the case when m ~ Sem r
-- and rearrange type arguments for simpler typeapplication
-- in use.
absorbWithSem :: forall p (x :: (Type -> Type) -> Type -> Type ->Type) d r a. d
              -> (forall s. R.Reifies s d :- p (x (Sem r) s))
              -> (p (Sem r) => Sem r a) -> Sem r a
absorbWithSem = using @d @x
{-# INLINEABLE absorbWithSem #-}
