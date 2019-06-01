{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module RandomFuSpec where

import           Polysemy
import           Polysemy.RandomFu

import           Test.Hspec
import           Control.Monad                 as M
import           Control.Monad.IO.Class         ( liftIO )

import qualified Data.Random                   as R
import qualified Data.Random.Source.PureMT     as R

getRandomInts :: Member RandomFu r => Int -> Sem r [Int]
getRandomInts nDraws =
  sampleRVar $ M.replicateM nDraws (R.uniform 0 (100 :: Int))

getRandomIntsMR :: R.MonadRandom m => Int -> m [Int]
getRandomIntsMR nDraws =
  R.sample $ M.replicateM nDraws (R.uniform 0 (100 :: Int))

randomListsDifferent :: Member RandomFu r => Int -> Sem r Bool
randomListsDifferent nDraws = do
  a <- getRandomInts nDraws
  b <- getRandomInts nDraws
  return (a /= b)

randomListsDifferentMR :: R.MonadRandom m => Int -> m Bool
randomListsDifferentMR nDraws = do
  a <- getRandomIntsMR nDraws
  b <- getRandomIntsMR nDraws
  return (a /= b)

------------------------------------------------------------------------------

spec :: Spec
spec = describe "RandomFu" $ do
  it
      "Should produce [3, 78, 53, 41, 56], 5 psuedo-random Ints seeded from the same seed on each test."
    $ do
        result <- runM . runRandomIOPureMT (R.pureMT 1) $ getRandomInts 5
        result `shouldBe` [3, 78, 53, 41, 56]

  it
      "Should produce [3, 78, 53, 41, 56], 5 psuedo-random Ints seeded from the same seed on each test. Absorbing MonadRandom."
    $ do
        result <-
          runM
          . runRandomIOPureMT (R.pureMT 1)
          $ absorbMonadRandom
          $ getRandomIntsMR 5

        result `shouldBe` [3, 78, 53, 41, 56]


  it "Should produce two distinct sets of psuedo-random Ints." $ do
    result <- runM . runRandomIO $ randomListsDifferent 5
    result `shouldBe` True

  it
      "Should produce two distinct sets of psuedo-random Ints (absorber version)."
    $ do
        result <-
          runM . runRandomIO $ absorbMonadRandom $ randomListsDifferentMR 5
        result `shouldBe` True

