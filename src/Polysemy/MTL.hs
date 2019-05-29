{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE UndecidableInstances        #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE UndecidableSuperClasses     #-}
{-# LANGUAGE AllowAmbiguousTypes         #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Polysemy.MTL
  (
  -- * Types
    CanonicalEffect
  , ConstrainedAction
  , ReifiableConstraint1
  , SemDictionary

  -- * constraint-polymorphic absorber
  , absorb
  
  -- * constraint-monomorphic absorbers
  , absorbReader
  , absorbState
  , absorbWriter
  , absorbError
  )
where


import qualified Control.Monad.Reader.Class as S
import qualified Control.Monad.State.Class as S
import qualified Control.Monad.Writer.Class as S
import qualified Control.Monad.Error.Class as S
import qualified Data.Constraint as C
import           Data.Constraint ((:-),(\\))
import qualified Data.Constraint.Unsafe as C
import           Data.Proxy (Proxy (..))
import qualified Data.Reflection as R
import           Data.Kind (Type, Constraint)

import           Polysemy
import           Polysemy.Reader
import           Polysemy.Writer
import           Polysemy.State
import           Polysemy.Error

{- | Open type-family mapping a single constraint of the form @(Type -> Type) -> Constraint@, e.g., @MonadState s@,
to a polysemy effect which can be used to re-interpret that constraint, e.g., 'State s'.
-}
type family CanonicalEffect (p :: (Type -> Type) -> Constraint) :: (Type -> Type) -> Type -> Type

type instance  CanonicalEffect (S.MonadReader env) = Reader env
type instance  CanonicalEffect (S.MonadWriter w)   = Writer w
type instance  CanonicalEffect (S.MonadState s)    = State s
type instance  CanonicalEffect (S.MonadError e)    = Error e


{- | A newtype wrapper for a monadic action, parameterized by
a constraint, @p@ on a @(Type -> Type)@ (e.g., a monad); @m@, a specific
@(Type -> Type)@; and a polysemy effect type-list @r@. With "Data.Reflection"
we can create instances of @p (ConstrainedAction p m r)@ using functions from
@Sem r@.
-}
newtype ConstrainedAction (p :: (Type -> Type) -> Constraint) (m :: Type -> Type) (s :: Type) (x :: Type)
  = ConstrainedAction
    { action :: m x
    } deriving (Functor, Applicative, Monad)

{- | For a constraint to be "absorbable" by @Sem r@, there needs to be an instance of this class,
containing the dictionary signatures as a record of functions and the
reflected entailment of @p (ConstrainedAction p m r)@ from the reified dictionary.
-}
class ReifiableConstraint1 p where
  data Dict (p :: (Type -> Type) -> Constraint) (m :: Type -> Type)
  reifiedInstance :: Monad m => R.Reifies s (Dict p m) :- p (ConstrainedAction p m s)

{- | This class contains an instance of the dictionary for some set of effects
parameterized by a polysemt effect list @r@.
Typically, you would write this instance for any @r@
satisfying the constraint that the "canonical" effect is a member.  But you
could also use it to discharge constraints which require multiple polysemy effects.
-}
class ReifiableConstraint1 p => SemDictionary p r where
  semDictionary :: Dict p (Sem r)

-- | Given a reifiable constraint, and a dictionary to use, discharge the constraint.
using :: forall p m a. (Monad m, ReifiableConstraint1 p) => Dict p m -> (p m => m a) -> m a
using d m =
  R.reify d $ \(_ :: Proxy s) -> m \\ C.trans (C.unsafeCoerceConstraint :: (p (ConstrainedAction p m s) :- p m)) reifiedInstance

{- | Given a "canonical" dictionary for @p@ using the polysemy effects in @r@,
discharge the constraint @p@.
-}
absorb :: forall p r a. SemDictionary p r => (p (Sem r) => Sem r a) -> Sem r a
absorb = (using @p) semDictionary

------------------------------------------------------------------------------
absorbReader :: forall i r a. Member (Reader i) r => (S.MonadReader i (Sem r) => Sem r a) -> Sem r a
absorbReader f = absorb @(S.MonadReader i) f --using (MonadReader ask local) f

instance ReifiableConstraint1 (S.MonadReader i) where
  data Dict (S.MonadReader i) m = MonadReader
    { ask_ :: m i
    , local_ :: forall a. (i -> i) -> m a -> m a
    }
  reifiedInstance = C.Sub C.Dict

instance ( Monad m
         , R.Reifies s' (Dict (S.MonadReader i) m)
         ) => S.MonadReader i (ConstrainedAction (S.MonadReader i) m s') where
  ask = ConstrainedAction $ ask_ $ R.reflect $ Proxy @s'
  local f m = ConstrainedAction $ local_ (R.reflect $ Proxy @s') f $ action m

instance Member (Reader i) r => SemDictionary (S.MonadReader i) r where
  semDictionary = MonadReader ask local
  
------------------------------------------------------------------------------
absorbState :: forall s r a. Member (State s) r => (S.MonadState s (Sem r) => Sem r a) -> Sem r a
absorbState f = absorb @(S.MonadState s) f -- using (MonadState get put) f

instance ReifiableConstraint1 (S.MonadState s) where
  data Dict (S.MonadState s) m = MonadState
    { get_ :: m s
    , put_ :: s -> m ()
    }
  reifiedInstance = C.Sub C.Dict

instance ( Monad m
         , R.Reifies s' (Dict (S.MonadState s) m)
         ) => S.MonadState s (ConstrainedAction (S.MonadState s) m s') where
  get = ConstrainedAction $ get_ $ R.reflect $ Proxy @s'
  put s = ConstrainedAction $ put_ (R.reflect $ Proxy @s') s


instance Member (State s) r => SemDictionary (S.MonadState s) r where
  semDictionary = MonadState get put

--------------------------------------------------------------------------------
absorbWriter :: forall w r a. (Monoid w, Member (Writer w) r) => (S.MonadWriter w (Sem r) => Sem r a) -> Sem r a
absorbWriter = absorb @(S.MonadWriter w)

instance Monoid w => ReifiableConstraint1 (S.MonadWriter w) where
  data Dict (S.MonadWriter w) m = MonadWriter
    { tell_ :: w -> m ()
    , listen_ :: forall a. m a -> m (a, w)
    , pass_ :: forall a. m (a, w -> w) -> m a 
    }
  reifiedInstance = C.Sub C.Dict

instance ( Monad m
         , Monoid w
         , R.Reifies s' (Dict (S.MonadWriter w) m)
         ) => S.MonadWriter w (ConstrainedAction (S.MonadWriter w) m s') where
  tell w = ConstrainedAction $ tell_ (R.reflect $ Proxy @s') w
  listen x = ConstrainedAction $ listen_ (R.reflect $ Proxy @s') (action x)
  pass x = ConstrainedAction $ pass_ (R.reflect $ Proxy @s') (action x)

{- This one requires a little work since the polysemy writer is a bit different from the
mtl-standard one
-} 
instance (Monoid w, Member (Writer w) r) => SemDictionary (S.MonadWriter w) r where
  semDictionary = MonadWriter tell semListen semPass where
    semListen = fmap (\(x,y) -> (y,x)) . listen
    semPass :: Member (Writer w) r => Sem r (a, w -> w) -> Sem r a 
    semPass x = do
      (a, f) <- x
      censor f (return a)

--------------------------------------------------------------------------------
absorbError :: forall e r a. Member (Error e) r => (S.MonadError e (Sem r) => Sem r a) -> Sem r a
absorbError = absorb @(S.MonadError e)

instance ReifiableConstraint1 (S.MonadError e) where
  data Dict (S.MonadError e) m = MonadError
    { throwError_ :: forall a. e -> m a
    , catchError_ :: forall a. m a -> (e -> m a) -> m a
    }
  reifiedInstance = C.Sub C.Dict

instance ( Monad m
         , R.Reifies s' (Dict (S.MonadError e) m)
         ) => S.MonadError e (ConstrainedAction (S.MonadError e) m s') where
  throwError e = ConstrainedAction $ throwError_ (R.reflect $ Proxy @s') e
  catchError x f = ConstrainedAction $ catchError_ (R.reflect $ Proxy @s') (action x) (action . f)

instance Member (Error e) r => SemDictionary (S.MonadError e) r where
  semDictionary = MonadError throw catch 
