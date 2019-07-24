{-# LANGUAGE RecursiveDo #-}
module FinalSpec where

import Test.Hspec

import Control.Monad.State  hiding (MonadState(..), modify)
import Control.Monad.Except hiding (MonadError(..))
import Control.Monad.Writer hiding (MonadWriter(..), censor)

import Data.Either
import Data.IORef

import Polysemy

import Polysemy.Trace
import Polysemy.State

import Polysemy.Final.Async
import Polysemy.Final.Fixpoint
import Polysemy.Final.Error

import Polysemy.Reader
import Polysemy.Writer
import Polysemy.Final.MTL

data Node a = Node a (IORef (Node a))

mkNode :: (Member (Embed IO) r, Member Fixpoint r)
       => a
       -> Sem r (Node a)
mkNode a = mdo
  let nd = Node a p
  p <- embed $ newIORef nd
  return nd

linkNode :: Member (Embed IO) r
         => Node a
         -> Node a
         -> Sem r ()
linkNode (Node _ r) b =
  embed $ writeIORef r b

readNode :: Node a -> a
readNode (Node a _) = a

follow :: Member (Embed IO) r
       => Node a
       -> Sem r (Node a)
follow (Node _ ref) = embed $ readIORef ref

test1 :: IO (Either Int (String, Int, Maybe Int))
test1 = do
  ref <- newIORef "abra"
  runFinal
    . runStateIORef ref -- Order of these interpreters don't matter
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
  . runTraceList
  . runErrorInIOFinal
  . runAsyncFinal
  $ do
  fut <- async $ do
    trace "Global state semantics?"
  catch (trace "What's that?" *> throw ()) (\_ -> return ())
  _ <- await fut
  trace "Nothing at all."

test3 :: Int -> (Either Bool (([String], Int), Int), [Int])
test3 i =
  let
    g = do
      j  <- ask
      j' <- get
      tell [j, j']
      put (j' + 7)
      trace "message"
      when (j' == 1) $ throw True
      when (j' == 2) $ throw False
      return j
  in
    ($ i)
  . runWriterT
  . runExceptT
  . (`runStateT` 0)
  . runFinal
  . runTraceList -- Order of these interpreters don't matter
  . runWriterFinal
  . runStateFinal
  . runErrorFinal
  . runReaderFinal
  $ do
    ask >>= put
    res <-
      censor (++[777]) (local (+1) g)
      `catch`
      (\e -> trace "not" *> if e then throw e else return (-1))
    trace "received"
    j' <- get
    tell [j']
    return res

spec :: Spec
spec = do
  describe "Final on IO" $ do
    it "should terminate successfully, with no exceptions,\
        \ and have global state semantics on State." $ do
      res1 <- test1
      res1 `shouldSatisfy` isRight
      case res1 of
        Right (s, i, j) -> do
          i `shouldBe` 2
          j `shouldBe` Just 1
          s `shouldBe` "abrahadabra"
        _ -> pure ()

    it "should treat trace with local state semantics" $ do
      res2 <- test2
      res2 `shouldBe` (["Nothing at all."], Right ())

  describe "Final with MTL" $ do
    it "should all work without issue" $ do
      let (r, written) = test3 0
      written `shouldBe` [1,0,777,7]
      r `shouldSatisfy` isRight
      case r of
        Right ((lg, ret), st) -> do
          lg `shouldBe` ["message", "received"]
          ret `shouldBe` 1
          st `shouldBe` 7
        _ -> pure ()

    it "should fail, dropping trace, state, and censoring" $ do
      let (r, written) = test3 1
      r `shouldBe` Left True
      written `shouldBe` [2, 1]

    it "should catch exception, locally dropping trace and state, and not censor" $ do
      let (r, written) = test3 2
      written `shouldBe` [3,2,2]
      r `shouldSatisfy` isRight
      case r of
        Right ((lg, ret), st) -> do
          lg `shouldBe` ["not", "received"]
          ret `shouldBe` (-1)
          st `shouldBe` 2
        _ -> pure ()
