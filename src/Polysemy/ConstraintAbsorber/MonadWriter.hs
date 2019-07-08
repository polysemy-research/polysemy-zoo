{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE UndecidableInstances        #-}

module Polysemy.ConstraintAbsorber.MonadWriter
  ( absorbWriter
  ) where

import qualified Control.Monad.Writer.Class as S
import           Polysemy
import           Polysemy.ConstraintAbsorber
import           Polysemy.Writer


------------------------------------------------------------------------------
-- | Introduce a local 'S.MonadWriter' constraint on 'Sem' --- allowing it to
-- interop nicely with MTL.
--
-- @since 0.3.0.0
absorbWriter
    :: forall w r a
     . ( Monoid w
       , Member (Writer w) r
       )
    => (S.MonadWriter w (Sem r) => Sem r a)
       -- ^ A computation that requires an instance of 'S.MonadWriter' for
       -- 'Sem'. This might be something with type @'S.MonadWriter' w m => m a@.
    -> Sem r a
absorbWriter =
  let swapTuple (x,y) = (y,x)
      semTell = tell
      semListen :: Member (Writer w) r => Sem r b -> Sem r (b, w)
      semListen = fmap swapTuple . listen @w
      semPass :: Member (Writer w) r => Sem r (b, w -> w) -> Sem r b
      semPass = pass @w . fmap swapTuple
  in absorbWithSem @(S.MonadWriter _) @Action
     (WriterDict semTell semListen semPass)
     (Sub Dict)
{-# INLINEABLE absorbWriter #-}


------------------------------------------------------------------------------
-- | A dictionary of the functions we need to supply
-- to make an instance of Writer
data WriterDict w m = WriterDict
  { tell_ :: w -> m ()
  , listen_ :: forall a. m a -> m (a, w)
  , pass_ :: forall a. m (a, w -> w) -> m a
  }


------------------------------------------------------------------------------
-- | Wrapper for a monadic action with phantom
-- type parameter for reflection.
-- Locally defined so that the instance we are going
-- to build with reflection must be coherent, that is
-- there cannot be orphans.
newtype Action m s' a = Action { action :: m a }
  deriving (Functor, Applicative, Monad)


------------------------------------------------------------------------------
-- | Given a reifiable mtl Writer dictionary,
-- we can make an instance of @MonadWriter@ for the action
-- wrapped in @Action@.
instance ( Monad m
         , Monoid w
         , Reifies s' (WriterDict w m)
         ) => S.MonadWriter w (Action m s') where
  tell w = Action $ tell_ (reflect $ Proxy @s') w
  {-# INLINEABLE tell #-}
  listen x = Action $ listen_ (reflect $ Proxy @s') (action x)
  {-# INLINEABLE listen #-}
  pass x = Action $ pass_ (reflect $ Proxy @s') (action x)
  {-# INLINEABLE pass #-}

