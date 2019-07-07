module Polysemy.Final.Error
  (
    runErrorInIOFinal
  ) where

import Control.Exception
import Data.Typeable
import Polysemy
import Polysemy.Final
import Polysemy.Error hiding (throw, catch)

------------------------------------------------------------------------------
-- | Run an 'Error' effect as an 'IO' 'Exception'.
runErrorInIOFinal
    :: ( Typeable e
       , Member (Final IO) r
       )
    => Sem (Error e ': r) a
    -> Sem r (Either e a)
runErrorInIOFinal sem = withStrategic $ do
  m' <- runS (runErrorAsExcFinal sem)
  s <- getInitialStateS
  pure $
    either
      ((<$ s) . Left . unwrapExc)
      (fmap Right)
    <$> try m'

runErrorAsExcFinal
    :: forall e r a
    .  ( Typeable e
       , Member (Final IO) r
       )
    => Sem (Error e ': r) a
    -> Sem r a
runErrorAsExcFinal = interpretHFinal $ \case
  Throw e   -> pure $ throwIO $ WrappedExc e
  Catch m h -> do
    m' <- runS m
    h' <- bindS h
    s  <- getInitialStateS
    pure $ catch m' $ \(se :: WrappedExc e) ->
      h' (unwrapExc se <$ s)


newtype WrappedExc e = WrappedExc { unwrapExc :: e }
  deriving (Typeable)

instance Typeable e => Show (WrappedExc e) where
  show = mappend "WrappedExc: " . show . typeRep

instance (Typeable e) => Exception (WrappedExc e)
