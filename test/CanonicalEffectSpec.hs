{-# LANGUAGE OverloadedStrings #-}
module CanonicalEffectSpec where

import           Polysemy
import           Polysemy.Reader
import           Polysemy.Writer
import           Polysemy.State
import           Polysemy.Error
import           Polysemy.CanonicalEffect

import qualified Data.Text                     as T
import           Test.Hspec

import           Control.Monad                  ( replicateM_
                                                , when
                                                )
import qualified Control.Monad.Reader          as MTL
import qualified Control.Monad.Writer          as MTL
import qualified Control.Monad.State           as MTL
import qualified Control.Monad.Except          as MTL


{-
We could re-write these to use polysemy directly.  Imagine, though
that these come from external libraries so you can't so easily
re-write them.
-}
getEnvLength :: MTL.MonadReader T.Text m => m Int
getEnvLength = MTL.ask >>= return . T.length

replicateTell :: MTL.MonadWriter [Int] m => Int -> Int -> m ()
replicateTell n m = replicateM_ n $ MTL.tell [m]

retrieveAndUpdateN :: MTL.MonadState Int m => Int -> m Int
retrieveAndUpdateN n = do
  m <- MTL.get
  MTL.put n
  return m

-- this one is exceptionally boring
throwOnZero :: MTL.MonadError T.Text m => Int -> m Int
throwOnZero n = do
  when (n == 0) $ MTL.throwError "Zero!"
  return n


someOfAll
  :: (MTL.MonadReader T.Text m, MTL.MonadWriter [Int] m, MTL.MonadState Int m)
  => m T.Text
someOfAll = do
  n <- MTL.get
  MTL.tell [n]
  MTL.ask


spec :: Spec
spec = do
  describe "CanonicalEffect" $ do

    it
        "should return 10, the sum of lengths of the strings provided to run (\"Text1\") and then to local (\"Text2\")"
      $ do
          flip shouldBe 10 $ run . runReader "Text1" $ do
            a <- absorbReader getEnvLength
            b <- local (const "Text2") $ absorbReader getEnvLength
            return (a + b)

    it
        "should return the sum, after censoring, of all things told.  In this case, 16, the sum of \"init [1,5,5,5,5]\""
      $ do
          flip shouldBe 16 $ sum . fst . run . runWriter $ do
            tell [1]
            absorbWriter $ replicateTell 2 5
            censor init $ absorbWriter $ replicateTell 2 5

    it "Should return 0 (since 10 - (20 `div` 2) = 0)"
      $ flip shouldBe 0
      $ fst
      . run
      . runState 0
      $ do
          put 20
          n <- absorbState $ retrieveAndUpdateN 10
          modify (\m -> m - (n `div` 2))
          return ()

    it "should return (Left \"Zero!\")."
      $ flip shouldBe (Left "Zero!")
      $ run
      . runError
      $ absorbError
      $ throwOnZero 0

    let runRWS
          :: T.Text
          -> Int
          -> Sem '[Reader T.Text, State Int, Writer [Int]] a
          -> ([Int], (Int, a))
        runRWS env0 s0 = run . runWriter . runState s0 . runReader env0

    it "All of them, singly"
      $ flip shouldBe ([20, 20], (10, 6))
      $ runRWS "RunAll" 0
      $ do
          put 20
          n <- absorbState $ retrieveAndUpdateN 10
          absorbWriter $ replicateTell 2 n
          a <- absorbReader getEnvLength
          return a

    it "All of them, one absorber"
      $ flip shouldBe ([20, 20], (10, 6))
      $ runRWS "RunAll" 0
      $ do
          put 20
          n <- absorbRWS $ retrieveAndUpdateN 10
          absorbRWS $ replicateTell 2 n
          a <- absorbRWS getEnvLength
          return a

    it "absorb a stack"
      $ flip shouldBe ([10, 20], (20, "RunAll"))
      $ runRWS "RunAll" 0
      $ do
          put 20
          tell [10]
          absorbRWS someOfAll
