module ShiftSpec where

import Test.Hspec

import Data.IORef

import Polysemy
import Polysemy.Shift
import Polysemy.Error
import Polysemy.Reader
import Polysemy.Writer
import Polysemy.State
import Polysemy.Final.MTL

import Control.Monad.Cont(ContT(..))
import Control.Monad.State(StateT(..))


test1 :: Maybe (String, ())
test1 =
    run
  . runShiftPure
  . runReader (1 :: Int)
  . runWriter
  $ do
  censor (++"!") $ shift $ \c -> do
    _ <- local (+1) (c ())
    _ <- local (+1) (c ())
    tell "unimportant"
    local (+1) (c ())
  j <- ask
  tell (show j)

test2 :: Maybe (Either () ())
test2 =
    run
  . runShiftPure
  . runError
  $ do
  shift $ \_ -> do
    throw ()

test3 :: Maybe (Either () ())
test3 =
    run
  . runShiftPure
  . runError
  $ do
  r <- reset' $ shift $ \_ -> do
    throw ()
  case r of
    Just r' -> abort r'
    _       -> pure ()

test4 :: IO (Maybe Int)
test4 = do
  ref <- newIORef 1
  runM
   . runShiftM
   . runStateIORef ref
   $ do
    shift $ \c -> do
      _ <- c ()
      _ <- c ()
      c ()
    modify (+1)
    get

test5 :: (Maybe Int, Int)
test5 =
    ($ 1)
  . (`runStateT` 1)
  . (`runContT` (pure . Just))
  . runFinal
  . embedToFinal
  . stateToEmbed
  . shiftToFinal
  . readerToFinal
  $ do
  shift $ \c -> do
    _ <- local (+1) (c ())
    _ <- local (+2) (c ())
    local (+3) (c ())
  i <- ask
  modify (+i)
  get

test6 :: (Int, Maybe Int)
test6 =
     run
   . runState 1
   . runShiftUnsafe
   $ do
    shift $ \c -> do
      _ <- c ()
      _ <- c ()
      c ()
    modify (+1)
    get

spec :: Spec
spec = do
  describe "runShiftPure" $ do
    it "should only tell once, censor once, and\
       \ local should have no effect on the continuation." $
      test1 `shouldBe` Just ("!1", ())

    it "should fail from failing locally" $ do
      test2 `shouldBe` Nothing

    it "should recover from failing locally" $ do
      test3 `shouldBe` Just (Right ())

  describe "runShiftM" $ do
    it "should modify multiple times with runStateIORef" $ do
      res <- test4
      res `shouldBe` Just 4

  describe "shiftToFinal" $ do
    it "should modify multiple times with runStateFinal\
        \ and should be able to apply local to continuation" $
      test5 `shouldBe` (Just 10, 10)

  describe "runShiftUnsafe" $ do
    it "should modify multiple times with runState\
        \ run after it" $
      test6 `shouldBe` (4, Just 4)
