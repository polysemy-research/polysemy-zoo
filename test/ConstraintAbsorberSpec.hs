{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


module ConstraintAbsorberSpec where

import           Polysemy
import           Polysemy.Reader
import           Polysemy.Writer
import           Polysemy.State
import           Polysemy.Error

import           Polysemy.ConstraintAbsorber.MonadReader
import           Polysemy.ConstraintAbsorber.MonadState
import           Polysemy.ConstraintAbsorber.MonadWriter
import           Polysemy.ConstraintAbsorber.MonadError
import           Polysemy.ConstraintAbsorber.MonadCatch

import           Test.Hspec
import qualified Control.Monad                 as M
import           Data.Maybe                     ( fromMaybe )

import qualified Control.Monad.Reader.Class    as S
import qualified Control.Monad.Writer.Class    as S
import qualified Control.Monad.State.Class     as S
import qualified Control.Monad.Error.Class     as S
import qualified Control.Monad.Catch           as S


{-
We could re-write these to use polysemy directly.  Imagine, though
that these come from external libraries so you can't so easily
re-write them.
-}
getEnvLength :: S.MonadReader String m => m Int
getEnvLength = S.ask >>= return . length

replicateTell :: S.MonadWriter [Int] m => Int -> Int -> m ()
replicateTell n m = M.replicateM_ n $ S.tell [m]

retrieveAndUpdateN :: S.MonadState Int m => Int -> m Int
retrieveAndUpdateN n = do
  m <- S.get
  S.put n
  return m

-- this one is exceptionally boring
throwOnZero :: S.MonadError String m => Int -> m Int
throwOnZero n = do
  M.when (n == 0) $ S.throwError "Zero!"
  return n

data MyException = ZeroException | NegativeException | UnknownException deriving (Show, Eq)

instance S.Exception MyException

-- a different error handling option
throwOnZeroAndNegative :: S.MonadThrow m => Int -> m Int
throwOnZeroAndNegative n = do
  M.when (n == 0) $ S.throwM ZeroException
  M.when (n < 0) $ S.throwM NegativeException
  return n

-- this is dumb but it makes the point. 
handleZero :: S.MonadThrow m => MyException -> m Int
handleZero ZeroException = return 0
handleZero e             = S.throwM e


someOfAll
  :: (S.MonadReader String m, S.MonadWriter [Int] m, S.MonadState Int m)
  => m String
someOfAll = do
  n <- S.get
  S.tell [n]
  S.ask
------------------------------------------------------------------------------

spec :: Spec
spec = describe "ConstraintAbsorber" $ do
  it
      (  "should absorb reader twice, thus returning 9, "
      ++ "the sum of lengths of the strings provided to run (\"Text\")"
      ++ " and then to local (\"Text2\")"
      )
    $ do
        flip shouldBe 9 . run . runReader "Text" $ do
          a <- absorbReader getEnvLength
          b <- local (const "Text2") $ absorbReader getEnvLength
          return (a + b)

  it
      (  "should return the sum, after censoring, of all things told."
      ++ " In this case, 16, the sum of \"init [1,5,5,5,5]\""
      )
    $ do
        flip shouldBe 16 . sum @[] . fst . run . runWriter $ do
          tell [1]
          absorbWriter $ replicateTell 2 5
          censor init $ absorbWriter $ replicateTell 2 5

  it "same as above but with absorbWriter on the outside of the do block" $ do
    flip shouldBe 16 . sum . fst . run . runWriter $ absorbWriter $ do
      S.tell [1]
      replicateTell 2 5
      S.pass $ do
        x <- replicateTell 2 5
        return (x, init)

  it "Should return 0 (since 10 - (20 `div` 2) = 0)" $ do
    flip shouldBe 0 . fst . run . runState 0 $ do
      put 20
      n <- absorbState $ retrieveAndUpdateN 10
      modify (\m -> m - (n `div` 2))
      return ()

  it "should return (Right 1)." $ do
    flip shouldBe (Right 1) . run . runError $ absorbError $ throwOnZero 1
  it "should return (Left \"Zero!\")." $ do
    flip shouldBe (Left "Zero!") . run . runError $ absorbError $ throwOnZero 0

  let toMyException = fromMaybe UnknownException
  it "should return (Right 1)." $ do
    flip shouldBe (Right 1)
      . run
      . runMonadCatch toMyException
      $ absorbMonadThrow
      $ throwOnZeroAndNegative 1
  it "should return (Left ZeroException)." $ do
    flip shouldBe (Left ZeroException)
      . run
      . runMonadCatch toMyException
      $ absorbMonadThrow
      $ throwOnZeroAndNegative 0
  it "should return (Left \"ZeroException\")." $ do
    flip shouldBe (Left "ZeroException")
      . run
      . runMonadCatchAsText
      $ absorbMonadThrow
      $ throwOnZeroAndNegative 0
  it "should return (Right 0)." $ do
    flip shouldBe (Right 0)
      . run
      . runMonadCatch toMyException
      $ absorbMonadCatch
      $ S.catch (throwOnZeroAndNegative 0) handleZero
  it "should return (Left NegativeException)." $ do
    flip shouldBe (Left NegativeException)
      . run
      . runMonadCatch toMyException
      $ absorbMonadCatch
      $ S.catch (throwOnZeroAndNegative (-1)) handleZero

  let runRWS
        :: String
        -> Int
        -> Sem '[Reader String, State Int, Writer [Int]] a
        -> ([Int], (Int, a))
      runRWS env0 s0 = run . runWriter . runState s0 . runReader env0

  it "All of them, singly" $ do
    flip shouldBe ([20, 20], (10, 6)) . runRWS "RunAll" 0 $ do
      put 20
      n <- absorbState $ retrieveAndUpdateN 10
      absorbWriter $ replicateTell 2 n
      a <- absorbReader getEnvLength
      return a

  let
    absorbRWS
      :: (Monoid w, Members '[Reader env, Writer w, State s] r)
      => (  ( S.MonadReader env (Sem r)
           , S.MonadWriter w (Sem r)
           , S.MonadState s (Sem r)
           )
         => Sem r a
         )
      -> Sem r a
    absorbRWS x = absorbReader $ absorbWriter $ absorbState x

  it "All of them, one absorber" $ do
    flip shouldBe ([20, 20], (10, 6)) . runRWS "RunAll" 0 $ do
      put 20
      n <- absorbRWS $ retrieveAndUpdateN 10
      absorbRWS $ replicateTell 2 n
      a <- absorbRWS getEnvLength
      return a

  it "All of them, one absorber, absorbRWS outside do block." $ do
    flip shouldBe ([20, 20], (10, 6)) . runRWS "RunAll" 0 $ absorbRWS $ do
      S.put 20
      n <- retrieveAndUpdateN 10
      replicateTell 2 n
      a <- getEnvLength
      return a

  it "absorb a stack" $ do
    flip shouldBe ([10, 20], (20, "RunAll")) . runRWS "RunAll" 0 $ do
      put 20
      tell [10]
      absorbRWS someOfAll

  it "absorb a stack, absorb outside do." $ do
    flip shouldBe ([10, 20], (20, "RunAll"))
      . runRWS "RunAll" 0
      $ absorbRWS
      $ do
          S.put 20
          S.tell [10]
          someOfAll
