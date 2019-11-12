module Polysemy.Input.Streaming where

import           Data.Functor.Of
import           Polysemy
import           Polysemy.Input
import           Polysemy.State
import qualified Streaming as S
import qualified Streaming.Prelude as S


runInputViaStream
    :: S.Stream (Of i) (Sem r) ()
    -> InterpreterFor (Input (Maybe i)) r
runInputViaStream stream
  = evalState (Just stream)
  . reinterpret ( \Input ->
      get >>= \case
        Nothing -> pure Nothing
        Just s ->
          raise (S.inspect s) >>= \case
            Left () -> pure Nothing
            Right (i :> s') -> do
              put $ Just s'
              pure $ Just i
  )


runInputViaInfiniteStream
    :: forall i r
     . (forall x. S.Stream (Of i) (Sem r) x)
    -> InterpreterFor (Input i) r
runInputViaInfiniteStream stream
  = evalState (stream :: S.Stream (Of i) (Sem r) ())
  . reinterpret ( \Input -> do
      s <- get
      raise (S.inspect s) >>= \case
        Left () -> error "runInputViaInfiniteStream: impossible!"
        Right (i :> s') -> do
          put s'
          pure i
  )


yieldInput :: Member (Input i) r => S.Stream (Of i) (Sem r) ()
yieldInput = S.lift input >>= S.yield

