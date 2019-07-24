module FloodgateSpec where

import Test.Hspec
import Polysemy
import Polysemy.Floodgate
import Polysemy.Trace

spec :: Spec
spec = describe "Floodgate" $ do
  it "should delay held traces until release" $ do
    let (ts, n) = run . runTraceList . runFloodgate $ do
          hold $ trace "first1"
          hold $ trace "first2"
          trace "not held"
          hold $ trace "second"
          trace "not held again"
          hold $ trace "third"
          release
          trace "finished"
          pure $ id @Int 17

    n `shouldBe` 17
    ts `shouldBe`
      [ "not held"
      , "not held again"
      , "first1"
      , "first2"
      , "second"
      , "third"
      , "finished"
      ]
