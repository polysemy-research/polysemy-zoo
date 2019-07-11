module CaptureSpec where

import Test.Hspec

import Polysemy
import Polysemy.Capture
import Polysemy.Writer
import Polysemy.Reader
import Polysemy.Error

test1 :: (String, Maybe ())
test1 =
    run
  . runReader (1 :: Int)
  . runWriter
  . runCapture
  $ do
  capture $ \c -> do
    _ <- local (+1) (c ())
    _ <- censor (show . (+2) . (read :: String -> Int)) (c ())
    tell "important"
    local (+3) (c ())
  j <- ask
  tell (show j)

test2 :: (String, Maybe ())
test2 =
    run
  . runReader (1 :: Int)
  . runWriter
  . runCapture
  $ do
  delimit $ capture $ \c -> do
    _ <- local (+1) (c ())
    _ <- local (+2) (c ())
    tell "important"
    local (+3) (c ())
  j <- ask
  tell (show j)

test3 :: (String, Maybe ())
test3 =
    run
  . runReader (1 :: Int)
  . runWriter
  . runCapture
  $ do
  censor (++"!") $ capture $ \c -> do
    _ <- local (+1) (c ())
    _ <- local (+2) (c ())
    tell "important"
    local (+3) (c ())
  j <- ask
  tell (show j)

test4 :: Maybe (String, ())
test4 =
    run
  . runReader (1 :: Int)
  . runCapture
  . runWriter
  $ do
  capture $ \c -> do
    _ <- local (+1) (c ())
    _ <- local (+2) (c ())
    tell "important"
    local (+3) (c ())
  j <- ask
  tell (show j)

test5 :: Maybe (Either () ())
test5 =
    run
  . runCapture
  . runError
  $ do
  capture $ \_ -> do
    throw ()

test6 :: Maybe (Either () ())
test6 =
    run
  . runCapture
  . runError
  $ do
  r <- delimit' $ capture $ \_ -> do
    throw ()
  case r of
    Just g -> return g
    _      -> return ()

spec :: Spec
spec = do
  describe "runCapture" $ do
    it "should have global state semantics, and\
     \ have higher-order effects affect continuations" $
      test1 `shouldBe` ("23important4", Just ())

    it "should have global state semantics, but\
     \ 'delimit' should delimit the continuation." $
      test2 `shouldBe` ("important1", Just ())

    it "should have global state semantics, and\
        \ 'censor' should delimit the continuation" $
      test3 `shouldBe` ("important!1", Just ())

    it "should treat writer with local state semantics, but\
       \ reader with global state semantics." $
      test4 `shouldBe` Just ("4", ())

    it "should fail from failing locally" $
      test5 `shouldBe` Nothing

    it "should recover from failing locally" $
      test6 `shouldBe` Just (Right ())
