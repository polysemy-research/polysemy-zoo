module HListSpec where

import           Polysemy
import           Polysemy.State
import           Polysemy.Reader
import           Polysemy.Input
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

inputProgram :: ( Member (Input String) r
                , Member (Input Int) r
                , Member (Input Bool) r
                , Members [Input String, Input Int, Input Bool] r
                    ~ Members (Inputs [String, Int, Bool]) r
                ) => Sem r (Int, String, Bool)
inputProgram = do
  a <- input @Int
  b <- input @String
  c <- input @Bool
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

  describe "runConstInput" $ do
    let original = runConstInput 5 . runConstInput "test"
                                   . runConstInput True $ inputProgram
        new = runConstInputs (True ::: "test" ::: 5 ::: HNil) inputProgram

    it "should be equivalent to composed runConstInput" $ do
      run original `shouldBe` run new
