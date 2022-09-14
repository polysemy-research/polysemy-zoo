module Polysemy.Reader.Compact where

import           Polysemy
import           Polysemy.Reader

import           GHC.Compact

-----------------------------------------------------------------------------
-- | Run a 'Reader' effect by compacting a value; otherwise behaves as normal.
-- Useful for 'Reader' effects which provide a large structure.
runReaderWithCompacted
    :: forall r i a
     . Member (Embed IO) r
    => i
    -> Sem (Reader i ': r) a
    -> Sem r a
runReaderWithCompacted i sem = do
    compacted <- embed (compactWithSharing i)
    runReaderWithExistingCompacted compacted sem
{-# INLINE runReaderWithCompacted #-}

-----------------------------------------------------------------------------
-- | Run a 'Reader' effect with a value in a compact region. Will not add 
-- 'local' values to the existing region, but will create a new region for it.
runReaderWithExistingCompacted
    :: forall r i a
     . Member (Embed IO) r
    => Compact i
    -> Sem (Reader i ': r) a
    -> Sem r a
runReaderWithExistingCompacted i = interpretH $ \case
    Ask       -> pureT (getCompact i)
    Local f m -> do
        mm <- runT m
        let transformed = f (getCompact i)
        raise $ runReaderWithCompacted transformed mm
{-# INLINE runReaderWithExistingCompacted #-}
