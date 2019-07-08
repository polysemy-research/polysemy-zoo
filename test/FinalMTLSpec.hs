module FinalMTLSpec where

import Data.Either

import Test.Hspec

import Control.Monad.State  hiding (MonadState(..))
import Control.Monad.Except hiding (MonadError(..))
import Control.Monad.Writer hiding (MonadWriter(..), censor)

import Polysemy.Error
import Polysemy.Reader
import Polysemy.State
import Polysemy.Writer
import Polysemy.Final.MTL
import Polysemy.Trace

test :: Int -> (Either Bool (([String], Int), Int), [Int])
test i =
  let
    g = do
      j  <- ask
      j' <- get
      tell [j, j']
      put (j' + 7)
      trace "message"
      when (j' == 1) $ throw True
      when (j' == 2) $ throw False
      return j
  in
    ($ i)
  . runWriterT
  . runExceptT
  . (`runStateT` 0)
  . runFinal
  . runTraceAsList -- Order of these interpreters don't matter
  . runWriterFinal
  . runStateFinal
  . runErrorFinal
  . runReaderFinal
  $ do
    ask >>= put
    res <-
      censor (++[777]) (local (+1) g)
      `catch`
      (\e -> trace "not" *> if e then throw e else return (-1))
    trace "received"
    j' <- get
    tell [j']
    return res

spec :: Spec
spec = describe "Final with MTL" $ do
  it "should all work without issue" $ do
    let (r, written) = test 0
    written `shouldBe` [1,0,777,7]
    r `shouldSatisfy` isRight
    case r of
      Right ((lg, ret), st) -> do
        lg `shouldBe` ["message", "received"]
        ret `shouldBe` 1
        st `shouldBe` 7
      _ -> pure ()

  it "should fail, dropping trace, state, and censoring" $ do
    let (r, written) = test 1
    r `shouldBe` Left True
    written `shouldBe` [2, 1]

  it "should catch exception, locally dropping trace and state, and not censor" $ do
    let (r, written) = test 2
    written `shouldBe` [3,2,2]
    r `shouldSatisfy` isRight
    case r of
      Right ((lg, ret), st) -> do
        lg `shouldBe` ["not", "received"]
        ret `shouldBe` (-1)
        st `shouldBe` 2
      _ -> pure ()
