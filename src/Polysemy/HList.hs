{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.HList
        ( -- * Data
          HList(..)
        , Readers
        , States

          -- * Interpretations
        , runReaders
        , runStates
        )
where

import Polysemy
import Polysemy.State
import Polysemy.Reader
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
-- | Turns a list of Types into a list of Readers.
type Readers (a :: [Type]) = TMap Reader a

------------------------------------------------------------------------------
-- | Given a list of values, this funcion will interpret each value as if it
-- were a Reader. For example, @runReaders (5 ::: "Test" ::: True ::: HNil)@
-- is equivalent to @runReader True . runReader "Test" . runReader 5@.
runReaders :: HList t -> Sem (TConcat (Readers t) r) a -> Sem r a
runReaders (a ::: as) = runReaders as . runReader a
runReaders HNil       = id

------------------------------------------------------------------------------
-- | Turns a list of Types into a list of States.
type States (a :: [Type]) = TMap State a

-- | Given a list of values, this funcion will interpret each value as if it
-- were a State. For example, @runStates (5 ::: "Test" ::: True ::: HNil)@
-- is equivalent to @runState True . runState "Test" . runState 5@. Note that
-- runStates will throw away the returned state value.
runStates :: HList t -> Sem (TConcat (States t) r) a -> Sem r a
runStates (a ::: as) = fmap snd . runStates as . runState a
runStates HNil       = id
