{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances       #-}
{-|
Module      : PolySemy.RandomFu
Description : Polysemy random-fu effect

Polysemy "random-fu" effect.
This can be run in a few ways:
1. Directly in 'IO'
2. Using any 'Data.Random.RandomSource' from "random-fu"
3. In 'IO', using a given 'Data.Random.Source.PureMT' source. ('IO' is used to put the source in an 'IORef')


Also, its presence in the Polysemy Union @r@,
gives @Sem r@ a "MonadRandom" instance (from "random-fu"). This is an Orphan instance.
-}

module Polysemy.RandomFu
  (
    -- * Effect
    RandomFu (..)

    -- * Actions
  , sampleRVar
  , getRandomPrim
  , sampleDist

  -- * Interpretations
  , runRandomSource
  , runRandomIO
  , runRandomIOPureMT

  -- * MTL helpers
  , absorbMonadRandom
  )
where

import           Polysemy
import           Polysemy.MTL

import           Data.IORef                     ( newIORef )
import qualified Data.Random                   as R
import qualified Data.Random.Source            as R
import           Data.Random.Source.Std         (StdRandom(..))
import qualified Data.Random.Internal.Source   as R
import qualified Data.Random.Source.PureMT     as R
--import qualified Data.Reflection               as RE
import qualified System.Random                 as SR
import           Control.Monad.IO.Class         ( MonadIO(..) )


------------------------------------------------------------------------------
{- | An effect capable of sampling from a "random-fu" RVar or generating a
single random-variate of any type, @t@ with a
@Data.Random.Prim t@ constructor, currently one of @Word8@, @Word16@,
@Word32@, @Word64@, @Double@ or N-byte integer.
-}
data RandomFu m r where
  SampleRVar ::  R.RVar t -> RandomFu m t
  GetRandomPrim :: R.Prim t -> RandomFu m t

makeSem ''RandomFu

------------------------------------------------------------------------------
-- | use the 'RandomFu` effect to sample from a "random-fu" @Distribution@.
sampleDist
  :: (Member RandomFu r, R.Distribution d t) => d t -> Sem r t
sampleDist = sampleRVar . R.rvar

------------------------------------------------------------------------------
-- | Run a 'Random' effect using a given 'R.RandomSource'
runRandomSource
  :: forall s r a
   . R.RandomSource (Sem r) s
  => s
  -> Sem (RandomFu ': r) a
  -> Sem r a
runRandomSource source = interpret $ \case
    SampleRVar    rv -> R.runRVar (R.sample rv) source
    GetRandomPrim pt -> R.runRVar (R.getRandomPrim pt) source
{-# INLINE runRandomSource #-}

------------------------------------------------------------------------------
-- | Run a 'Random` effect by using the default "random-fu" 'IO' source
runRandomIO
  :: forall r a
   . MonadIO (Sem r)
  => Sem (RandomFu ': r) a
  -> Sem r a
runRandomIO = interpret $ \case
    SampleRVar    rv -> liftIO $ R.sample rv
    GetRandomPrim pt -> liftIO $ R.getRandomPrim pt
{-# INLINE runRandomIO #-}

------------------------------------------------------------------------------
-- | Run in 'IO', using the given 'R.PureMT' source, stored in an 'IORef'
runRandomIOPureMT
  :: MonadIO (Sem r)
  => R.PureMT
  -> Sem (RandomFu ': r) a
  -> Sem r a
runRandomIOPureMT source re =
  liftIO (newIORef source) >>= flip runRandomSource re
{-# INLINE runRandomIOPureMT #-}

absorbMonadRandom
  :: Member RandomFu r => (R.MonadRandom (Sem r) => Sem r a) -> Sem r a
absorbMonadRandom = absorb @R.MonadRandom
{-# INLINE absorbMonadRandom #-}

instance ReifiableConstraint1 (R.MonadRandom) where
  data Dict1 R.MonadRandom m = MonadRandom
    {
      getRandomPrim_ :: forall t. R.Prim t -> m t
    }
  reifiedInstance = Sub Dict


$(R.monadRandom [d|
        instance ( Monad m
                 , Reifies s' (Dict1 R.MonadRandom m)
                 ) => R.MonadRandom (ConstrainedAction R.MonadRandom m s') where
            getRandomPrim t = ConstrainedAction $ getRandomPrim_ (reflect $ Proxy @s') t
    |])

instance Member RandomFu r => IsCanonicalEffect R.MonadRandom r where
  canonicalDictionary = MonadRandom getRandomPrim
