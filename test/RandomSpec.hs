module RandomSpec where

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
