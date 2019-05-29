{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MTLSpec where

import           Polysemy
import           Polysemy.Reader
import           Polysemy.Writer
import           Polysemy.State
import           Polysemy.Error
import           Polysemy.MTL

import qualified Data.Text                     as T
import           Test.Hspec

import           Control.Monad                  ( replicateM_
                                                , when
                                                )
import qualified Control.Monad.Reader.Class    as S
import qualified Control.Monad.Writer.Class    as S
import qualified Control.Monad.State.Class     as S
import qualified Control.Monad.Error.Class     as S


{-
We could re-write these to use polysemy directly.  Imagine, though
that these come from external libraries so you can't so easily
re-write them.
-}
getEnvLength :: S.MonadReader T.Text m => m Int
getEnvLength = S.ask >>= return . T.length

replicateTell :: S.MonadWriter [Int] m => Int -> Int -> m ()
replicateTell n m = replicateM_ n $ S.tell [m]

retrieveAndUpdateN :: S.MonadState Int m => Int -> m Int
retrieveAndUpdateN n = do
  m <- S.get
  S.put n
  return m

-- this one is exceptionally boring
throwOnZero :: S.MonadError T.Text m => Int -> m Int
throwOnZero n = do
  when (n == 0) $ S.throwError "Zero!"
  return n

someOfAll
  :: (S.MonadReader T.Text m, S.MonadWriter [Int] m, S.MonadState Int m)
  => m T.Text
someOfAll = do
  n <- S.get
  S.tell [n]
  S.ask

spec :: Spec
spec = do
  describe "MTL" $ do

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

    let
      absorbRWS
        :: forall env w s r a
         . (Monoid w, Members '[Reader env, Writer w, State s] r)
        => (  ( S.MonadReader env (Sem r)
             , S.MonadWriter w (Sem r)
             , S.MonadState s (Sem r)
             )
           => Sem r a
           )
        -> Sem r a
      absorbRWS x =
        absorb @(S.MonadReader env)
          $ absorb @(S.MonadWriter w)
          $ absorb @(S.MonadState s) x

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
