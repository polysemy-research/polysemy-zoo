{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
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
    Random (..)

    -- * Actions
  , sampleRVar
  , getRandomPrim
  , sampleDist

  -- * Interpretations
  , runRandomSource
  , runRandomIO
  , runRandomIOPureMT
  )
where

import           Polysemy                      

import           Data.IORef                     ( newIORef )
import qualified Data.Random                   as R
import qualified Data.Random.Source            as R
import           Data.Random.Source.Std         (StdRandom(..))
import qualified Data.Random.Internal.Source   as R
import qualified Data.Random.Source.PureMT     as R
import qualified System.Random                 as SR
import           Control.Monad.IO.Class         ( MonadIO(..) )

------------------------------------------------------------------------------
{- | An effect capable of sampling from a "random-fu" RVar or generating a
single random-variate of any type, @t@ with a
@Data.Random.Prim t@ constructor, currently one of @Word8@, @Word16@,
@Word32@, @Word64@, @Double@ or N-byte integer.
-}
data Random m r where
  SampleRVar ::  R.RVar t -> Random m t
  GetRandomPrim :: R.Prim t -> Random m t

makeSem ''Random

------------------------------------------------------------------------------
-- | use the 'Random` effect to sample from a "random-fu" @Distribution@.
sampleDist
  :: (Member Random r, R.Distribution d t) => d t -> Sem r t
sampleDist = sampleRVar . R.rvar

------------------------------------------------------------------------------
-- | Run a 'Random' effect using a given 'R.RandomSource'
runRandomSource
  :: forall s r a
   . R.RandomSource (Sem r) s
  => s
  -> Sem (Random ': r) a
  -> Sem r a
runRandomSource source = interpret f
 where
  f :: forall m x . (Random m x -> Sem r x)
  f r = case r of
    SampleRVar    rv -> R.runRVar (R.sample rv) source
    GetRandomPrim pt -> R.runRVar (R.getRandomPrim pt) source
{-# INLINE runRandomSource #-}

------------------------------------------------------------------------------
-- | Run a 'Random` effect by using the default "random-fu" 'IO' source
runRandomIO
  :: forall r a
   . MonadIO (Sem r)
  => Sem (Random ': r) a
  -> Sem r a
runRandomIO = interpret f
 where
  f :: forall m x . (Random m x -> Sem r x)
  f r = case r of
    SampleRVar    rv -> liftIO $ R.sample rv
    GetRandomPrim pt -> liftIO $ R.getRandomPrim pt
{-# INLINE runRandomIO #-}

------------------------------------------------------------------------------
-- | Run in 'IO', using the given 'R.PureMT' source, stored in an 'IORef'
runRandomIOPureMT
  :: MonadIO (Sem r)
  => R.PureMT
  -> Sem (Random ': r) a
  -> Sem r a
runRandomIOPureMT source re =
  liftIO (newIORef source) >>= flip runRandomSource re
{-# INLINE runRandomIOPureMT #-}

-- | Orphan instance of 'R.MonadRandom'
$(R.monadRandom [d|
        instance Member Random r => R.MonadRandom (Sem r) where
            getRandomPrim = getRandomPrim
    |])
