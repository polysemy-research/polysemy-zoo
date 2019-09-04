{-# LANGUAGE RecursiveDo #-}
module RevStateSpec where

import Test.Hspec

import Data.Functor.Identity

import Polysemy
import Polysemy.State
import Polysemy.RevState
import Polysemy.EndState
import Polysemy.Fixpoint

runStateCircular :: (Member (State s) r, Member Fixpoint r)
                 => Sem (RevState s ': r) a
                 -> Sem r (s, a)
runStateCircular sem = runEndState $ do
  s <- getEndState
  raise $ runRevState s sem

test :: String
test =
    fst
  . snd
  . runIdentity
  . runFinal
  . fixpointToFinal
  . runState ""
  . runStateCircular
  $ do
   modify (++"time ")
   revModify (++"circle")
   modify (++"is ")
   revModify (++"flat ")
   modify (++"a ")

spec :: Spec
spec = do
  describe "runRevState" $ do
    it "should thread state backwards \
       \and work together with EndState" $
      test `shouldBe` "time is a flat circle"
