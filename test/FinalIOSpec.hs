{-# LANGUAGE RecursiveDo #-}
module FinalIOSpec where

import Test.Hspec

import Data.Either
import Data.IORef

import Polysemy

import Polysemy.State
import Polysemy.Trace
import Polysemy.Final.Async
import Polysemy.Final.Fixpoint
import Polysemy.Final.Error

data Node a = Node a (IORef (Node a))

mkNode :: (Member (Lift IO) r, Member Fixpoint r)
       => a
       -> Sem r (Node a)
mkNode a = mdo
  let nd = Node a p
  p <- sendM $ newIORef nd
  return nd

linkNode :: Member (Lift IO) r
         => Node a
         -> Node a
         -> Sem r ()
linkNode (Node _ r) b =
  sendM $ writeIORef r b

readNode :: Node a -> a
readNode (Node a _) = a

follow :: Member (Lift IO) r
       => Node a
       -> Sem r (Node a)
follow (Node _ ref) = sendM $ readIORef ref

test1 :: IO (Either Int (String, Int, Maybe Int))
test1 = do
  ref <- newIORef "abra"
  runFinal
    . runStateInIORef ref -- Order of these interpreters don't matter
    . runErrorInIOFinal
    . runFixpointFinal
    . runAsyncFinal
     $ do
     n1 <- mkNode 1
     n2 <- mkNode 2
     linkNode n2 n1
     aw <- async $ do
       linkNode n1 n2
       modify (++"hadabra")
       n2' <- follow n2
       throw (readNode n2')
     m <- await aw `catch` (\s -> return $ Just s)
     n1' <- follow n1
     s <- get
     return (s, readNode n1', m)

test2 :: IO ([String], Either () ())
test2 =
    runFinal
  . runTraceAsList
  . runErrorInIOFinal
  . runAsyncFinal
  $ do
  fut <- async $ do
    trace "Global state semantics?"
  catch (trace "What's that?" *> throw ()) (\_ -> return ())
  _ <- await fut
  trace "Nothing at all."

spec :: Spec
spec = describe "Final on IO" $ do
  res1 <- runIO test1
  it "should terminate successfully, with no exceptions,\
      \ and have global state semantics on State." $ do
    res1 `shouldSatisfy` isRight
    case res1 of
      Right (s, i, j) -> do
        i `shouldBe` 2
        j `shouldBe` Just 1
        s `shouldBe` "abrahadabra"
      _ -> pure ()

  res2 <- runIO test2
  it "should treat trace with local state semantics" $ do
    res2 `shouldBe` (["Nothing at all."], Right ())
