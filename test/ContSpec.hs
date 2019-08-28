module ContSpec where

import Test.Hspec

import Data.IORef

import Polysemy
import Polysemy.Cont
import Polysemy.Error
import Polysemy.Reader
import Polysemy.Writer
import Polysemy.State
import Polysemy.Trace
import Polysemy.Final.MTL

import qualified Control.Monad.State as S
import qualified Control.Monad.Cont as C

test1 :: (String, Int)
test1 =
    run
  . runContPure
  . runReader 1
  . runWriter
  $ do
  i <- censor (++"!") $ local (+1) $ callCC $ \c -> do
    i <- local (+1) ask
    tell "unimportant"
    local (+1) (c i)
  tell (show i)
  j <- ask
  return j

test2 :: (String, ())
test2 =
    run
  . runContPure
  . runReader (1 :: Int)
  . runWriter
  $ do
  i <- censor (++"!") $ local (+1) $ callCC $ \_ -> do
    i <- local (+1) ask
    tell "important"
    return i
  tell (show i)
  return ()

test3 :: Either () ()
test3 =
     run
   . runContPure
   . runError
   $ catch (callCC $ \_ -> throw ()) (\_ -> pure ())

stateTest :: (Member (State Int) r, Member (Cont ref) r)
          => Sem r Int
stateTest = do
  i <- get
  put (i + 1)
  callCC $ \c -> do
    j <- get
    put (j + 1)
    c ()
  get

test4 :: (Int, Int)
test4 =
    (`S.runState` 1)
  . runM
  . runContM
  . stateToEmbed
  $ stateTest

test5 :: IO (Int, Int)
test5 = do
  ref <- newIORef 1

  r <-  runM
      . runContM
      . runStateIORef ref
      $ stateTest
  s' <- readIORef ref
  return (r, s')

test6 :: (Int, Int)
test6 =
    run
  . runState 1
  . runContUnsafe
  $ stateTest

test7 :: ([String], String)
test7 =
    (`C.runCont` id)
  . runFinal
  . runTraceList
  . runReader ""
  . contToFinal
  $ do
  j <- local (++".") $ callCC $ \c -> do
    j <- ask
    trace "Global state semantics?"
    local (\_ -> "What's that?") (c j)
  i <- local (++"Nothing") ask
  trace $ i
  callCC $ \_ ->
    trace "at"
  trace "all."
  return j

test8 :: Int
test8 =
     ($ 1)
   . (`C.runContT` pure)
   . runFinal
   . readerToFinal
   . contToFinal
   $ do
  callCC $ \c ->
    local (+1) (c ())
  ask

spec :: Spec
spec = do
  describe "runContPure" $ do
    it "should work with higher-order effects if not applied on continuations\
          \ and discard local state" $
      test1 `shouldBe` ("!3", 1)

    it "should not discard local state if continuation is never invoked" $
      test2 `shouldBe` ("important!3", ())

    it "should catch exception within callCC" $
      test3 `shouldBe` Right ()

  describe "runContM" $ do
    it "should have global state semantics with stateToEmbed" $
      test4 `shouldBe` (3, 3)

    it "should have global state semantics with runStateIORef" $ do
      r <- test5
      r `shouldBe` (3, 3)

  describe "contToFinal" $ do
    it "should work just like runContPure/M." $
      test7 `shouldBe` (["Nothing", "at", "all."], ".")

    it "should be able to apply local to continuation" $
      test8 `shouldBe` 2

  describe "runContUnsafe" $ do
    it "should work with and have global state semantics with runState\
       \ run after it" $
      test6 `shouldBe` (3, 3)
