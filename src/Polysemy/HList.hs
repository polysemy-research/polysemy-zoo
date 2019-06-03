{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.HList
        ( -- * Data
          HList(..)
        , Readers
        , States
        , Inputs

          -- * Interpretations
        , runReaders
        , runStates
        , runConstInputs
        , runSeveral
        )
where

import Polysemy
import Polysemy.State
import Polysemy.Reader
import Polysemy.Input
import Data.Kind

------------------------------------------------------------------------------
-- | A list capable of storing values of different types. Like the Sem type,
-- it uses a type level list to keep track of what's stored inside. Creating a
-- HList looks like @1 ::: "test" ::: True ::: HNil@.
infixr 5 :::
data HList a where
    HNil  :: HList '[]
    (:::) :: a -> HList (b :: [Type]) -> HList (a ': b)

------------------------------------------------------------------------------
-- | A map function over type level lists.
type family TMap (f :: a -> b) (xs :: [a]) where
    TMap _ '[]       = '[]
    TMap f (x ': xs) = f x ': TMap f xs

------------------------------------------------------------------------------
-- | Like ++ but at the type level.
type family TConcat (a :: [t]) (b :: [t]) where
    TConcat '[] b = b
    TConcat (a ': as) b = a ': TConcat as b

------------------------------------------------------------------------------
-- | A helper function for building new runners which accept HLists intsead of
-- individual elements. If you would normally write @f 5 . f "Text" . f True@
-- then this function can turn that into
-- @runSeveral f (True ::: "Text" ::: 5 ::: HNil)@
runSeveral
    :: (forall r k x. k -> Sem (e k ': r) x -> Sem r x)
    -> HList t
    -> Sem (TConcat (TMap e t) r) a
    -> Sem r a
runSeveral f (a ::: as) = runSeveral f as . f a
runSeveral _ HNil       = id

------------------------------------------------------------------------------
-- | Turns a list of Types into a list of Readers.
type Readers (a :: [Type]) = TMap Reader a

------------------------------------------------------------------------------
-- | Given a list of values, this funcion will interpret each value as if it
-- were a Reader. For example, @runReaders (5 ::: "Test" ::: True ::: HNil)@
-- is equivalent to @runReader True . runReader "Test" . runReader 5@.
runReaders :: HList t -> Sem (TConcat (Readers t) r) a -> Sem r a
runReaders = runSeveral runReader

------------------------------------------------------------------------------
-- | Turns a list of Types into a list of States.
type States (a :: [Type]) = TMap State a

-- | Given a list of values, this funcion will interpret each value as if it
-- were a State. For example, @runStates (5 ::: "Test" ::: True ::: HNil)@
-- is roughly equivalent to @runState True . runState "Test" . runState 5@.
-- The only difference being that runStates will throw away the returned
-- state values.
runStates :: HList t -> Sem (TConcat (States t) r) a -> Sem r a
runStates = runSeveral (fmap (fmap snd) . runState)


------------------------------------------------------------------------------
-- | Turns a list of Types into a list of Inputs.
type Inputs (a :: [Type]) = TMap Input a

-- | Given a list of values, this funcion will interpret each value as if it
-- were a constant input. For example,
-- @runConstInput (5 ::: "Test" ::: True ::: HNil)@ is equivalent to
-- @runConstInput True . runConstInput "Test" . runConstInput 5@.
runConstInputs :: HList t -> Sem (TConcat (Inputs t) r) a -> Sem r a
runConstInputs = runSeveral runConstInput
