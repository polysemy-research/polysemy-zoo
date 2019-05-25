{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-|
Module      : PolySemy.CanonicalEffects
Description : Polysemy Class for "absorbing" mtl-style constraints.

The CanonicalEffect class represents a choice of polysemy
interpretation of standard mtl effects as well as a vehicle
for writing similar "constraint absorbers" for other effects
with mtl-style constraints.  This class is only useful for
absorbing single effects.

Consider a function of the type
f :: MonadReader env m => Int -> m a

It can clearly be "handled by a Polysemy Reader effect
but the signature requires an instance of the mtl
reader class.
-}

module Polysemy.CanonicalEffect
  ( CanonicalEffect(..)
  )
where

import           Polysemy
import           Polysemy.Reader
import           Polysemy.Writer
import           Polysemy.State
import           Polysemy.Error
import           Data.Kind                      ( Type
                                                , Constraint
                                                )

import qualified Control.Monad.Reader          as MTL
import qualified Control.Monad.Writer          as MTL
import qualified Control.Monad.State           as MTL
import qualified Control.Monad.Except          as MTL

class CanonicalEffect (c :: (Type -> Type) -> Constraint) (e :: (Type -> Type) -> Type -> Type) where
  absorb :: Member e r => (forall m. c m => m a) -> Sem r a

-- Reader
newtype AbsorbReader env r a = AbsorbReader { unAbsorbReader :: Sem r a } deriving (Functor, Applicative, Monad)

instance Member (Reader env) r => MTL.MonadReader env (AbsorbReader env r) where
  ask = AbsorbReader $ ask @env
  local f = AbsorbReader . local @env f . unAbsorbReader

instance CanonicalEffect (MTL.MonadReader env) (Reader env) where
  absorb = unAbsorbReader :: AbsorbReader env r a -> Sem r a

-- Writer
newtype AbsorbWriter o r a = AbsorbWriter { unAbsorbWriter :: Sem r a } deriving (Functor, Applicative, Monad)

instance (Monoid o, Member (Writer o) r) => MTL.MonadWriter o (AbsorbWriter o r) where
  tell = AbsorbWriter . tell
  listen = AbsorbWriter . fmap (\(x,y) -> (y,x)) . listen . unAbsorbWriter
  pass x = AbsorbWriter $ do
    (a, f) <- unAbsorbWriter x
    censor f (return a)

instance Monoid o => CanonicalEffect (MTL.MonadWriter o) (Writer o) where
  absorb = unAbsorbWriter :: AbsorbWriter o r a -> Sem r a

-- State
newtype AbsorbState s r a = AbsorbState { unAbsorbState :: Sem r a } deriving (Functor, Applicative, Monad)

instance Member (State s) r => MTL.MonadState s (AbsorbState s r) where
  get = AbsorbState get
  put = AbsorbState . put

instance CanonicalEffect (MTL.MonadState s) (State s) where
  absorb = unAbsorbState :: AbsorbState s r a -> Sem r a


-- Error 
newtype AbsorbError e r a = AbsorbError { unAbsorbError :: Sem r a } deriving (Functor, Applicative, Monad)

instance Member (Error e) r => MTL.MonadError e (AbsorbError e r) where
  throwError = AbsorbError . throw
  catchError a h = AbsorbError $ catch (unAbsorbError a) (unAbsorbError . h)

instance CanonicalEffect (MTL.MonadError e) (Error e) where
  absorb = unAbsorbError :: AbsorbError e r a -> Sem r a

