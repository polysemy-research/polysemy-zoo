{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Random
  ( -- * Effect
    Random (..)

    -- * Actions
  , random
  , randomR

    -- * Interpretations
  , runRandom
  , runRandomIO

    -- * Helpers
  , distributed
  , oneOf
  , sample
  , sampleR
  , weighted
  ) where

import           Data.List (genericReplicate)
import           Data.List.NonEmpty as NonEmpty ((!!), NonEmpty((:|)))
import           Numeric.Natural (Natural)
import           Polysemy
import           Polysemy.State
import qualified System.Random as R

------------------------------------------------------------------------------
-- | An effect capable of providing 'R.Random' values.
data Random m a where
  Random :: R.Uniform x => Random m x
  RandomR :: R.UniformRange x => (x, x) -> Random m x

makeSem_ ''Random

------------------------------------------------------------------------------
-- | Yield a value, randomly sampled from the uniform distribution over all values of the given type.
-- /e.g./ 'p <- random @Bool'
random :: forall x r.
          (R.Uniform x
          ,Member Random r) =>
          Sem r x

------------------------------------------------------------------------------
-- | Yield a value, randomly sampled from the uniform distribution over the given inclusive range.
-- /e.g./ 'p <- random @Int (-10, 10)'
randomR :: forall x r.
           (R.UniformRange x
           ,Member Random r) =>
           (x, x) -> Sem r x

------------------------------------------------------------------------------
-- | Run a 'Random' effect with an explicit 'R.RandomGen'.
runRandom
    :: forall q r a
     . R.RandomGen q
    => q
    -> Sem (Random ': r) a
    -> Sem r (q, a)
runRandom q = runState q . reinterpret (\case
  Random -> do
    ~(a, q') <- gets @q R.uniform
    put q'
    pure a
  RandomR r -> do
    ~(a, q') <- gets @q $ R.uniformR r
    put q'
    pure a
                                       )
{-# INLINE runRandom #-}


------------------------------------------------------------------------------
-- | Run a 'Random' effect by using the 'IO' random generator.
runRandomIO :: Member (Embed IO) r => Sem (Random ': r) a -> Sem r a
runRandomIO m = do
  q <- embed @IO R.newStdGen
  snd <$> runRandom q m
{-# INLINE runRandomIO #-}


------------------------------------------------------------------------------
-- | Pick (uniformly) randomly from a (finite) non-empty list.
oneOf :: forall a r.
         (Member Random r) =>
         NonEmpty a -> Sem r a
oneOf xs = do i <- randomR (0, length xs - 1)
              return $ xs NonEmpty.!! i

------------------------------------------------------------------------------
-- | Pick randomly from a finite non-empty list, using weight annotations.
-- Behavior is undefined if all weights are zero.
weighted :: forall a r.
            (Member Random r) =>
            NonEmpty (Natural, a) -> Sem r a
weighted xs = consume xs <$> randomR (0, (sum $ fst <$> xs) - 1)

------------------------------------------------------------------------------
-- | Pick randomly from a non-empty possibly-infinite list, using normalized weight annotations.
-- The requirement that all weights be 0-1 (inclusive) and that they sum to 1 is not checked!
distributed :: forall a w r.
               (Num w
               ,Ord w
               ,R.UniformRange w
               ,Member Random r) =>
               NonEmpty (w, a) -> Sem r a
distributed xs = consume xs <$> randomR (0, 1)


consume :: (Num w, Ord w) => NonEmpty (w, a) -> w -> a
consume ((_, x) :| []) _ = x
consume ((weight, x) :| (x' : xs)) threshold | threshold < weight = x
                                             | otherwise          = consume (x' :| xs) (threshold - weight)

------------------------------------------------------------------------------
-- | Generate n random values.
sample :: forall a i r.
          (Integral i,
           R.Uniform a,
           Member Random r) =>
          i -> Sem r [a]
sample n = sequence . genericReplicate n $ random

------------------------------------------------------------------------------
-- | Generate n random values in a range.
sampleR :: forall a i r.
           (Integral i,
            R.UniformRange a,
            Member Random r) =>
           i -> (a, a) -> Sem r [a]
sampleR n r = sequence . genericReplicate n $ randomR r

