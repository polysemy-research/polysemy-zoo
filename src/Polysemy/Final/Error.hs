module Polysemy.Final.Error
  (
    module Polysemy.Error
  , module Polysemy.Final
  , errorToIOFinal
  ) where

import           Control.Exception hiding (throw, catch)
import qualified Control.Exception as X
import           Data.Typeable (Typeable, typeRep)
import           Polysemy
import           Polysemy.Final
import           Polysemy.Error

------------------------------------------------------------------------------
-- | Run an 'Error' effect as an 'IO' 'Exception' through 'Final' 'IO'.
--
-- This can be used as an alternative to 'lowerError'
--
-- /Beware/: Effects that aren't interpreted in terms of 'IO'
-- will have local state semantics in regards to 'Error' effects
-- interpreted this way. See 'Final'.
errorToIOFinal
    :: ( Typeable e
       , Member (Final IO) r
       )
    => Sem (Error e ': r) a
    -> Sem r (Either e a)
errorToIOFinal sem = withStrategic $ do
  m' <- runS (runErrorAsExcFinal sem)
  s  <- getInitialStateS
  pure $
    either
      ((<$ s) . Left . unwrapExc)
      (fmap Right)
    <$> try m'
{-# INLINE errorToIOFinal #-}

runErrorAsExcFinal
    :: forall e r a
    .  ( Typeable e
       , Member (Final IO) r
       )
    => Sem (Error e ': r) a
    -> Sem r a
runErrorAsExcFinal = interpretFinal $ \case
  Throw e   -> pure $ throwIO $ WrappedExc e
  Catch m h -> do
    m' <- runS m
    h' <- bindS h
    s  <- getInitialStateS
    pure $ X.catch m' $ \(se :: WrappedExc e) ->
      h' (unwrapExc se <$ s)
{-# INLINE runErrorAsExcFinal #-}


newtype WrappedExc e = WrappedExc { unwrapExc :: e }
  deriving (Typeable)

instance Typeable e => Show (WrappedExc e) where
  show = mappend "WrappedExc: " . show . typeRep

instance (Typeable e) => Exception (WrappedExc e)
