module RandomSpec where

import Data.List (nub)
import Data.List.NonEmpty (NonEmpty((:|)), unfoldr)
import Data.Word (Word64)
import Test.Hspec
import Polysemy
import Polysemy.Random
import qualified System.Random as R

spec :: Spec
spec = describe "Random" $ do
  it "should transparently mirror the System.Random interface" $ do
    q0 <- R.initStdGen
    let (_, ns) = run . runRandom q0 . sequence . replicate 5 $
                    do int :: Int <- randomR (2, 200)
                       real :: Double <- randomR (2, 200)
                       word :: Word64 <- random
                       return (int, real, word)
        (reference, _) = foldr (\_ (ns', q') ->
                                  let (int :: Int, qi) = R.randomR (2, 200) q'
                                      (real :: Double, qr) = R.randomR (2, 200) qi
                                      (word :: Word64, qw) = R.random qr
                                  in ((int, real, word) : ns', qw)
                               ) ([], q0) (replicate 5 ())
    ns `shouldBe` reverse reference
  it "`oneOf` shouldn't crash." $ do
    five <- (nub <$>) . sequence . replicate 100 . runM . runRandomIO . oneOf $ (5 :: Int) :| []
    five `shouldBe` [5]
  it "`weighted` should give the only non-zero value." $ do
    five <- (nub <$>) . sequence . replicate 100 . runM . runRandomIO . weighted @Int $ (0, 4) :| [(1, 5), (0, 6)]
    five `shouldBe` [5]
  it "`distributed` should work on infinite input." $ do
    five <- (nub <$>) . sequence . replicate 100 . runM . runRandomIO . distributed @Int @Double $ unfoldr (\i -> ((i, 5), Just $ i / 2)) 0.5
    five `shouldBe` [5]
  it "`sample` should give the correct number of values" $ do
    five <- (length <$>) . runM . runRandomIO $ sample @Bool @Int 5
    five `shouldBe` 5
  it "`sampleR` should give the correct number of values" $ do
    five <- (length <$>) . runM . runRandomIO $ sampleR @Float @Int 5 (-10, 10)
    five `shouldBe` 5

