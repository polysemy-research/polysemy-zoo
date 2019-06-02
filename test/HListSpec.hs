module HListSpec where

import           Polysemy
import           Polysemy.State
import           Polysemy.Reader
import           Polysemy.HList
import           Test.Hspec


readerProgram :: ( Member (Reader String) r
                 , Member (Reader Int) r
                 , Member (Reader Bool) r
                 , Members [Reader String, Reader Int, Reader Bool] r
                     ~ Members (Readers [String, Int, Bool]) r
                 ) => Sem r (Int, String, Bool)
readerProgram = do
  a <- ask @Int
  b <- ask @String
  c <- ask @Bool
  pure $ (a, b, c)

stateProgram :: ( Member (State String) r
                , Member (State Int) r
                , Member (State Bool) r
                , Members [State String, State Int, State Bool] r
                    ~ Members (States [String, Int, Bool]) r
                ) => Sem r (Int, String, Bool)
stateProgram = do
  put @Int 5
  put "Changed"
  a <- get @Int
  b <- get @String
  c <- get @Bool
  pure $ (a, b, c)

spec :: Spec
spec = do
  describe "runReaders" $ do
    let original = runReader 5 . runReader "test" . runReader True $ readerProgram
        new = runReaders (True ::: "test" ::: 5 ::: HNil) readerProgram

    it "should be equivalent to composed runReader" $ do
      run original `shouldBe` run new

  describe "runStates" $ do
    let getResult = fmap $ snd . snd . snd
        original = getResult $ runState 5 . runState "test" . runState True $ stateProgram
        new = runStates (True ::: "test" ::: 5 ::: HNil) stateProgram

    it "should be equivalent to composed runState" $ do
      run original `shouldBe` run new
