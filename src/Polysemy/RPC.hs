{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell       #-}

module Polysemy.RPC where

import Data.Aeson
import Data.Proxy
import Data.Kind
import Polysemy
import Generics.Kind
import Generics.Kind.TH
import Polysemy.Internal.Tactics
import GHC.Generics (Meta (..))
import GHC.TypeLits


data RPC endpoint c m a where
  DoRPC :: endpoint -> c -> RPC endpoint c m c

makeSem ''RPC


data TTY m a where
  WriteTTY :: String -> TTY m ()
  ReadTTY :: TTY m String

deriveGenericK ''TTY
makeSem ''TTY


class (forall m x. GenericK (e m) (x :&&: LoT0)) => RPCable e where
  toRPC :: e m x -> Value

instance RPCable TTY where
  toRPC = gshow'



gshow' :: forall t. (GenericK t LoT0, GShow (RepK t), ReqsShow (RepK t) LoT0)
       => t -> Value
gshow' = gshow . fromK @_ @t @LoT0

class GShow (f :: LoT k -> *) where
  type ReqsShow f (x :: LoT k) :: Constraint
  gshow :: ReqsShow f x => f x -> Value

instance GShow (Field t) where
  type ReqsShow (Field t) x = ToJSON (Interpret t x)
  gshow (Field t) = toJSON t

instance GShow U1 where
  type ReqsShow U1 x = ()
  gshow U1 = Null

instance (GShow f, GShow g) => GShow (f :+: g) where
  type ReqsShow (f :+: g) x = (ReqsShow f x, ReqsShow g x)
  gshow (L1 f) = gshow f
  gshow (R1 g) = gshow g

instance (GShow f, GShow g) => GShow (f :*: g) where
  type ReqsShow (f :*: g) x = (ReqsShow f x, ReqsShow g x)
  gshow (f :*: g) = toJSON (gshow f, gshow g)

instance (GShow f, KnownSymbol name) => GShow (M1 _2 ('MetaCons name _1 _3) f) where
  type ReqsShow (M1 _2 ('MetaCons name _1 _3) f) x = ReqsShow f x
  gshow (M1 f) = toJSON (symbolVal $ Proxy @name, gshow f)

instance GShow f => GShow (M1 _2 ('MetaData _1 _3 _4 _5) f) where
  type ReqsShow (M1 _2 ('MetaData _1 _3 _4 _5) f) x = ReqsShow f x
  gshow (M1 f) = gshow f

instance GShow f => GShow (M1 _2 ('MetaSel _1 _3 _4 _5) f) where
  type ReqsShow (M1 _2 ('MetaSel _1 _3 _4 _5) f) x = ReqsShow f x
  gshow (M1 f) = gshow f

instance GShow f => GShow (c :=>: f) where
  type ReqsShow (c :=>: f) x = ReqsShow f x
  gshow (SuchThat f) = gshow f


hello :: Member TTY r => Sem r ()
hello = writeTTY "hello"


runEffectAsRPC
    :: ( RPCable e
       , Member (Lift IO) r
       )
    => endpoint
    -> Sem (e ': r) a
    -> Sem r a
runEffectAsRPC endpoint = interpretH $ \e -> do
  liftT $ do
    let s = toRPC e
    sendM $ print s
    undefined

main :: IO ()
main = runM . runEffectAsRPC @TTY () $ hello

