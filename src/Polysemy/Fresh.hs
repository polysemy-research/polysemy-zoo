{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, TemplateHaskell, Trustworthy #-}
module Polysemy.Fresh
  (-- * Effect
    Fresh(..)

    -- * Actions
  , fresh

    -- * Interpretations
  , freshToIO

    -- * Unsafe Interpretations
  , runFreshEnumUnsafe
  , runFreshUnsafePerformIO
  ) where

import Data.Unique

import Polysemy.Internal
import Polysemy.Internal.Union
import System.IO.Unsafe (unsafePerformIO)
import Polysemy
import Polysemy.State

-----------------------------------------------------------------------------
-- | An effect for creating unique objects which may be used as references,
-- a la 'Unique'. Polymorphic code making use of 'Fresh' is expected
-- to place constraints upon @uniq@ as necessary.
--
-- Any interpreter for 'Fresh' has the responsibilty of ensuring
-- that any call to 'fresh' produces an object that /never/
-- compares equal to an object produced by a previous call to 'fresh'.
data Fresh uniq m a where
  Fresh :: Fresh uniq m uniq

makeSem ''Fresh

-----------------------------------------------------------------------------
-- | Runs a 'Fresh' effect through generating 'Unique's using 'IO'.
freshToIO :: Member (Embed IO) r
          => Sem (Fresh Unique ': r) a
          -> Sem r a
freshToIO = interpret $ \Fresh -> embed newUnique
{-# INLINE freshToIO #-}

-----------------------------------------------------------------------------
-- | Run a 'Fresh' effect purely by specifying an 'Enum' to be used as the
-- type of unique objects.
--
-- __Beware:__ This is safe only if:
--
--   1. This is run after all interpreters which may revert local state
--      or produce multiple, inconsistent instances of local state.
--      This includes interpreters that may backtrack or produce multiple results
--      (such as 'Polysemy.Error.runError' or 'Polysemy.NonDet.runNonDet').
--
--   2. You don't use any interpreter which may cause the final monad
--      to revert local state or produce multiple, inconsistent instances of local
--      state. This includes certain 'Polysemy.Final.Final'/@lower-@ interpeters
--      such as 'Polysemy.Error.lowerError' or 'Polysemy.Final.MTL.errorToFinal',
--      as well as interpreters for 'Polysemy.Async.Async'.
--
-- Prefer 'freshToIO' whenever possible.
-- If you can't use 'runFreshEnumUnsafe' safely, nor use 'freshToIO', consider
-- 'runFreshUnsafePerformIO'.
runFreshEnumUnsafe :: forall n a r
                    . Enum n
                   => Sem (Fresh n ': r) a
                   -> Sem r a
runFreshEnumUnsafe =
    (fmap snd .)
  $ (runState @n (toEnum 0) .)
  $ reinterpret
  $ \Fresh -> do
    s <- get
    put $! succ s
    return s
{-# INLINE runFreshEnumUnsafe #-}

-----------------------------------------------------------------------------
-- | Runs a 'Fresh' effect through generating 'Unique's using
-- 'unsafePerformIO'.
--
-- Ironically, despite the fact that this uses 'unsafePerformIO', and
-- 'runFreshUnsafe' uses no unsafe operations whatsoever, this is still
-- typically safer to use than 'runFreshUnsafe', although 'runFreshUnsafe'
-- is perhaps more efficient.
--
-- The worst thing that this particular use of 'unsafePerformIO' could result
-- in is the loss of referential transparency, as rerunning an interpreter stack
-- using 'runFreshUnsafePerformIO' will create different 'Unique's. This should
-- never matter.
--
-- This could be potentially be less efficient than 'runFreshUnsafe'.
--
-- If you ever observe that multiple invocations of 'fresh' produce the same
-- 'Unique' under 'runFreshUnsafePerformIO', then open an issue over at the
-- GitHub repository.
runFreshUnsafePerformIO :: Sem (Fresh Unique ': r) a
                        -> Sem r a
runFreshUnsafePerformIO = usingSem $ \u ->
  case decomp u of
    Right (Weaving Fresh s _ ex _) -> do
      let !uniq = unsafePerformIO (newUnique' u)
          {-# NOINLINE uniq #-}
      pure $ ex (uniq <$ s)
    Left g -> liftSem (hoist runFreshUnsafePerformIO g)
-- KingoftheHomeless: I've tried very hard to prevent optimizations from
-- sharing the call to 'unsafePerformIO'.
-- The inlining of 'interpret' is so that I can give
-- 'u' to 'newUnique'', and thus prevent 'uniq' from floating outside the
-- lambda. This interpreter might even be safe to INLINE, but I'm not taking
-- any chances.
{-# NOINLINE runFreshUnsafePerformIO #-}

newUnique' :: Union (Fresh Unique ': r) (Sem (Fresh Unique ': r)) a -> IO Unique
newUnique' (Union _ _) = newUnique
{-# NOINLINE newUnique' #-}
