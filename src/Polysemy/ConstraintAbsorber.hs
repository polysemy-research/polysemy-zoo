{-# LANGUAGE AllowAmbiguousTypes         #-}
{-# LANGUAGE ConstraintKinds             #-}

module Polysemy.ConstraintAbsorber
  (
    -- * absorb-builder
   absorbWithSem
  
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
-- | Given a reifiable constraint @p,
-- discharge the constraint by providing a dictionary
-- and a "reified instance"
-- (often @Sub Dict@ will do)
-- Specialized to the case when m ~ Sem r
absorbWithSem :: forall (p :: (Type -> Type) -> Constraint) -- ^ Monadic Constraint to be absorbed
                        (x :: (Type -> Type) -> Type -> Type -> Type) -- ^ wrapper to avoid orphan instances
                        (d :: Type) -- ^ dictionary
                        (r :: [(Type -> Type) -> Type -> Type]) -- ^ Sem effect list
                        (a :: Type) 
                 . d
              -> (forall s. R.Reifies s d :- p (x (Sem r) s))
              -> (p (Sem r) => Sem r a) -> Sem r a
absorbWithSem d i m =  R.reify d $ \(_ :: Proxy (s :: Type)) -> m \\ C.trans
  (C.unsafeCoerceConstraint :: ((p (x m s) :- p m))) i
{-# INLINEABLE absorbWithSem #-}
