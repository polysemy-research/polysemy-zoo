module SeveralSpec where

import           Polysemy
import           Polysemy.State
import           Polysemy.Reader
import           Polysemy.Input
import           Polysemy.Several
import           Test.Hspec


readerProgram :: ( Member (Reader String) r
                 , Member (Reader Int) r
                 , Member (Reader Bool) r
                 , Members [Reader String, Reader Int, Reader Bool] r
                     ~ Members (TypeMap Reader [String, Int, Bool]) r
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
                    ~ Members (TypeMap State [String, Int, Bool]) r
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
                    ~ Members (TypeMap Input [String, Int, Bool]) r
                ) => Sem r (Int, String, Bool)
inputProgram = do
  a <- input @Int
  b <- input @String
  c <- input @Bool
  pure $ (a, b, c)


runReaders :: HList t -> Sem (TypeConcat (TypeMap Reader t) r) a -> Sem r a
runReaders = runSeveral runReader

runStates :: HList t -> Sem (TypeConcat (TypeMap State t) r) a -> Sem r a
runStates = runSeveral (fmap (fmap snd) . runState)

runConstInputs :: HList t -> Sem (TypeConcat (TypeMap Input t) r) a -> Sem r a
runConstInputs = runSeveral runInputConst

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

  describe "runInputConst" $ do
    let original = runInputConst 5 . runInputConst "test"
                                   . runInputConst True $ inputProgram
        new = runConstInputs (True ::: "test" ::: 5 ::: HNil) inputProgram

    it "should be equivalent to composed runInputConst" $ do
      run original `shouldBe` run new

