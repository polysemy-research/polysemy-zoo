module Polysemy.Final.More where

import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Union
import Polysemy.Final

------------------------------------------------------------------------------
-- | Run a @'Final' ('Sem' r)@ effect if the remaining effect stack is @r@.
--
-- This is sometimes useful for interpreters that make use of
-- 'reinterpret', 'raiseUnder', or any of their friends.
runFinalSem :: Sem (Final (Sem r) ': r) a -> Sem r a
runFinalSem = usingSem $ \u -> case decomp u of
  Right (Weaving (WithWeavingToFinal wav) s wv ex ins) ->
    ex <$> wav s (runFinalSem . wv) ins
  Left g -> liftSem (hoist runFinalSem g)
{-# INLINE runFinalSem #-}

------------------------------------------------------------------------------
-- | Run a @'Final' m@ effect by providing an explicit lowering function.
--
-- /Beware/: The lowering function may be invoked multiple times, so
-- __don't do any initialization work inside the lowering function__:
-- it will be duplicated.
lowerFinal :: Member (Embed m) r
           => (forall x. Sem r x -> m x)
           -> Sem (Final m ': r) a
           -> Sem r a
-- TODO(KingoftheHomeless): Write everything out for efficiency?
lowerFinal f = runFinalSem . finalToFinal embed f . raiseUnder
{-# INLINE lowerFinal #-}
