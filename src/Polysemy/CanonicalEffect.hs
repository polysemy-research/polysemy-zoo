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

Many Polysemy effects are isomoprhic to mtl effects.  In those cases
we ought to be able to "absorb" the mtl effect, as long as it's
specified by a typeclass constraint.  This module contains helpers
to do just that for Reader, Writer, State and Error.

See [test/CanonicalEffect.hs](https://github.com/isovector/polysemy-zoo/tree/master/test/CanonicalEffectSpec.hs) for examples of use.
-}

module Polysemy.CanonicalEffect
  (
    -- * Absorbers
    absorbReader
  , absorbWriter
  , absorbState
  , absorbError
  )
where

import           Polysemy
import           Polysemy.Reader
import           Polysemy.Writer
import           Polysemy.State
import           Polysemy.Error

import qualified Control.Monad.Reader          as MTL
import qualified Control.Monad.Writer          as MTL
import qualified Control.Monad.State           as MTL
import qualified Control.Monad.Except          as MTL


-- Reader
newtype AbsorbReader env r a = AbsorbReader { unAbsorbReader :: Sem r a } deriving (Functor, Applicative, Monad)

instance Member (Reader env) r => MTL.MonadReader env (AbsorbReader env r) where
  ask = AbsorbReader ask
  local f = AbsorbReader . local f . unAbsorbReader

absorbReader
  :: forall env m r a
   . Member (Reader env) r
  => (forall m . MTL.MonadReader env m => m a)
  -> Sem r a
absorbReader = unAbsorbReader :: AbsorbReader env r a -> Sem r a

-- Writer
newtype AbsorbWriter o r a = AbsorbWriter { unAbsorbWriter :: Sem r a } deriving (Functor, Applicative, Monad)

instance (Monoid o, Member (Writer o) r) => MTL.MonadWriter o (AbsorbWriter o r) where
  tell = AbsorbWriter . tell
  listen = AbsorbWriter . fmap (\(x,y) -> (y,x)) . listen . unAbsorbWriter
  pass x = AbsorbWriter $ do
    (a, f) <- unAbsorbWriter x
    censor f (return a)

absorbWriter
  :: forall w m r a
   . (Monoid w, Member (Writer w) r)
  => (forall m . MTL.MonadWriter w m => m a)
  -> Sem r a
absorbWriter = unAbsorbWriter :: AbsorbWriter w r a -> Sem r a


-- State
newtype AbsorbState s r a = AbsorbState { unAbsorbState :: Sem r a } deriving (Functor, Applicative, Monad)

instance Member (State s) r => MTL.MonadState s (AbsorbState s r) where
  get = AbsorbState get
  put = AbsorbState . put

absorbState
  :: forall s m r a
   . Member (State s) r
  => (forall m . MTL.MonadState s m => m a)
  -> Sem r a
absorbState = unAbsorbState :: AbsorbState s r a -> Sem r a

-- Error 
newtype AbsorbError e r a = AbsorbError { unAbsorbError :: Sem r a } deriving (Functor, Applicative, Monad)

instance Member (Error e) r => MTL.MonadError e (AbsorbError e r) where
  throwError = AbsorbError . throw
  catchError a h = AbsorbError $ catch (unAbsorbError a) (unAbsorbError . h)

absorbError
  :: forall e m r a
   . Member (Error e) r
  => (forall m . MTL.MonadError e m => m a)
  -> Sem r a
absorbError = unAbsorbError :: AbsorbError e r a -> Sem r a

