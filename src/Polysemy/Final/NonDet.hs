{-# LANGUAGE AllowAmbiguousTypes #-}
module Polysemy.Final.NonDet
  (
    module Polysemy.NonDet
  , nonDetToFinal
  ) where

import Control.Applicative

import Polysemy
import Polysemy.NonDet
import Polysemy.Final

-----------------------------------------------------------------------------
-- | Run an 'NonDet' effect through a final 'Alternative'
--
-- /Beware/: Effects that aren't interpreted in terms of the final
-- monad will have local state semantics in regards to 'NonDet' effects
-- interpreted this way. See 'Final'.
nonDetToFinal :: forall m r a
               . (Member (Final m) r, Alternative m)
              => Sem (NonDet ': r) a
              -> Sem r a
nonDetToFinal = interpretFinal @m $ \case
  Empty -> pure empty
  Choose left right -> do
    left'  <- runS left
    right' <- runS right
    pure $ left' <|> right'
{-# INLINE nonDetToFinal #-}
