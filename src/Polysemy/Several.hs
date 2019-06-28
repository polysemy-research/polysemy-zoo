{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Several
  ( -- * Data
    HList(..)
  , TypeMap
  , TypeConcat
  , runSeveral
  ) where

import Polysemy
import Data.Kind

------------------------------------------------------------------------------
-- | A list capable of storing values of different types. Like the Sem type,
-- it uses a type level list to keep track of what's stored inside. Creating an
-- HList looks like:
--
-- > 1 ::: "test" ::: True ::: HNil
infixr 5 :::
data HList a where
    HNil  :: HList '[]
    (:::) :: a -> HList (b :: [Type]) -> HList (a ': b)

------------------------------------------------------------------------------
-- | A map function over type level lists. For example, the following two
-- lines are equivalent:
--
-- > TypeMap Reader [Int, String, False]
-- > [Reader Int, Reader String, Reader Bool]
type family TypeMap (f :: a -> b) (xs :: [a]) where
    TypeMap _ '[]       = '[]
    TypeMap f (x ': xs) = f x ': TypeMap f xs

------------------------------------------------------------------------------
-- | Like ++ but at the type level. The following two lines are equivalent:
--
-- > TypeConcat [Int, String] [Bool]
-- > [Int, String, Bool]
type family TypeConcat (a :: [t]) (b :: [t]) where
    TypeConcat '[] b = b
    TypeConcat (a ': as) b = a ': TypeConcat as b

------------------------------------------------------------------------------
-- | A helper function for building new runners which accept HLists intsead of
-- individual elements. If you would normally write
--
-- > f 5 . f "Text" . f True
--
-- then this function can turn that into
--
-- > runSeveral f (True ::: "Text" ::: 5 ::: HNil)
--
-- For example, a runReaders function could be implemented as:
--
-- > runReaders :: HList t -> Sem (TypeConcat (TypeMap Reader t) r) a -> Sem r a
-- > runReaders = runSeveral runReader
--
-- Likewise, runStates could be the following if you didn't want the returned
-- state:
--
-- > runStates :: HList t -> Sem (TypeConcat (TypeMap State t) r) a -> Sem r a
-- > runStates = runSeveral (fmap (fmap snd) . runState)
runSeveral
    :: (forall r' k x. k -> Sem (e k ': r') x -> Sem r' x)
    -> HList t
    -> Sem (TypeConcat (TypeMap e t) r) a
    -> Sem r a
runSeveral f (a ::: as) = runSeveral f as . f a
runSeveral _ HNil       = id

