{-# LANGUAGE ImpredicativeTypes #-}

module Polysemy.IdempotentLowering
  ( (.@!)
  , nat
  , (.@@!)
  , nat'
  ) where

import Polysemy
import Data.Coerce

newtype Nat m n = Nat (∀ x. m x -> n x)

------------------------------------------------------------------------------
-- | This is just 'pure' but with a type specialised for lifting interpreters.
--
-- @since 0.1.1.0
nat :: Applicative base => (∀ x. m x -> n x) -> base (∀ x. m x -> n x)
nat f = pure $ coerce $ Nat f

newtype Nat' m n f = Nat' (∀ x. m x -> n (f x))

------------------------------------------------------------------------------
-- | 'nat'' is to 'nat' as '.@@!' is to '.@!'.
--
-- @since 0.1.1.0
nat' :: Applicative base => (∀ x. m x -> n (f x)) -> base (∀ x. m x -> n (f x))
nat' f = pure $ coerce $ Nat' f

------------------------------------------------------------------------------
-- | Like 'Polysemy..@', but useful for interpreters that wish to perform some
-- initialization before being run. Most of the time, you don't want to
-- duplicate this initialization every time your effect is lowered.
--
-- Consider an interpreter which wants to use an 'Data.IORef.IORef' to store
-- intermediary state. It might begin like this:
--
-- @
-- myIntepreter
--     :: 'Polysemy.Member' ('Polysemy.Lift' 'IO') r
--     => (∀ x. 'Polysemy.Sem' r x -> 'IO' x)
--     -> 'Polysemy.Sem' (MyEff ': r) a
--     -> 'Polysemy.Sem' r a
-- myInterpreter lower sem = do
--     ref <- 'Polysemy.sendM' $ 'Data.IORef.newIORef' 0
--     go ref sem
--   where
--     go ref = 'Polysemy.interpretH' $ \e -> ...
-- @
--
-- This interpreter will do the wrong thing when composed via 'Polysemy..@'. It
-- would have been correct if we didn't attempt to hide the creation of the
-- 'Data.IORef.IORef', but that's an unfortunate side-effect of wanting to hide
-- implementation details.
--
-- Instead, we can write @myInterpreter@ thusly:
--
-- @
-- myIntepreter
--     :: (∀ x. 'Polysemy.Sem' r x -> 'IO' x)
--     -> 'IO' (∀ a. 'Polysemy.Sem' (MyEff ': r) a -> 'Polysemy.Sem' r a)
-- myInterpreter lower = do
--     ref <- 'Data.IORef.newIORef' 0
--     'nat' $ 'Polysemy.interpretH' $ \e -> ...
-- @
--
-- and use '.@!' (rather than 'Polysemy..@') to compose these things together.
--
-- Note: you must enable @-XImpredicativeTypes@ to give the correct type to
-- @myInterpreter@ here. Don't worry, it's (probably) not as scary as it
-- sounds.
--
-- @since 0.1.1.0
(.@!)
    :: (Monad base, Monad m)
    => base (∀ x. Sem r x -> m x)
       -- ^ The lowering function, likely @nat runM@.
    -> ( (∀ x. Sem r x -> m x)
      -> base ( ∀ y
           . Sem (e ': r) y
          -> Sem r y
           )
       )
    -> base (∀ z. Sem (e ': r) z -> m z)
fi .@! gi = do
  f <- fi
  g <- gi f
  nat $ \z -> f $ g z
infixl 9 .@!


------------------------------------------------------------------------------
-- | Like '.@!', but for interpreters which change the resulting type --- eg.
-- 'Polysemy.Error.runErrorInIO'.
--
-- @since 0.1.1.0
(.@@!)
    :: (Monad base, Monad m)
    => base (∀ x. Sem r x -> m x)
       -- ^ The lowering function, likely @nat runM@.
    -> ( (∀ x. Sem r x -> m x)
      -> base ( ∀ y
           . Sem (e ': r) y
          -> Sem r (f y)
           )
       )
    -> base (∀ z. Sem (e ': r) z -> m (f z))
fi .@@! gi = do
  f <- fi
  g <- gi f
  nat' $ \z -> f $ g z
infixl 9 .@@!

