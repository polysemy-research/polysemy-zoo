{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module IdempotentLoweringSpec where

import Data.IORef
import Polysemy
import Polysemy.State
import Polysemy.IdempotentLowering
import Polysemy.Resource
import Test.Hspec


runStateInIO :: Member (Embed IO) r => s -> IO (∀ x. Sem (State s ': r) x -> Sem r x)
runStateInIO s = do
  ref <- newIORef s
  nat $ runStateIORef ref


test
    :: ( Member Resource r
       , Member (State Int) r
       )
    => Sem r Int
test = do
  bracket
    (modify (+1))
    (const $ modify (+1))
    (const $ modify (+1))
  get


spec :: Spec
spec = describe "Idempotent Lowering" $ do
  it "should persist an IORef through a bracket" $ do
    runIt <- nat runM .@! const (runStateInIO 0) .@! liftNat lowerResource
    result <- runIt test
    result `shouldBe` (3 :: Int)

