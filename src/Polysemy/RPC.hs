{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell       #-}

module Polysemy.RPC where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Kind
import Data.Proxy
import GHC.Generics (Meta (..))
import GHC.TypeLits
import Generics.Kind
import Generics.Kind.TH
import Polysemy
import Polysemy.Internal.Tactics
import Unsafe.Coerce


data RPC m a where
  DoRPC :: Value -> RPC m Value

makeSem ''RPC


data TTY m a where
  WriteTTY :: String -> TTY m ()
  ReadTTY :: TTY m String

deriving instance Show (TTY m a)

deriveGenericK ''TTY
makeSem ''TTY


class RPCable e where
  toRPC :: e m x -> Value

  default toRPC
      :: forall m x
       . (GenericK (e m x) LoT0, GShow (RepK (e m x)) 'LoT0)
      => e m x
      -> Value
  toRPC = gtoRPC . fromK @_ @(e m x) @LoT0

  fromRPC :: Value -> Result (e m x)
  default fromRPC
      :: forall m x
       . (GenericK (e m x) LoT0, GShow (RepK (e m x)) 'LoT0)
      => Value
      -> Result (e m x)
  fromRPC v = parse (fmap (toK @_ @(e m x) @LoT0) . gfromRPC) v

deriving instance RPCable TTY


class GShow (f :: LoT k -> *) (x :: LoT k) where
  gtoRPC :: f x -> Value
  gfromRPC :: Value -> Parser (f x)

instance (ToJSON (Interpret t x), FromJSON (Interpret t x)) => GShow (Field t) x where
  gtoRPC (Field t) = toJSON t
  gfromRPC = fmap Field . parseJSON

instance GShow U1 x where
  gtoRPC U1 = Null
  gfromRPC Null = pure U1
  gfromRPC _    = empty

instance (GShow f x, GShow g x) => GShow (f :+: g) x where
  gtoRPC (L1 f) = gtoRPC f
  gtoRPC (R1 g) = gtoRPC g
  gfromRPC v = L1 <$> gfromRPC v
           <|> R1 <$> gfromRPC v

instance (GShow f x, GShow g x) => GShow (f :*: g) x where
  gtoRPC (f :*: g) = toJSON (gtoRPC f, gtoRPC g)
  gfromRPC v = do
    (f, g) <- parseJSON v
    f' <- gfromRPC f
    g' <- gfromRPC g
    pure (f' :*: g')

instance (GShow f x, KnownSymbol name) => GShow (M1 _2 ('MetaCons name _1 _3) f) x where
  gtoRPC (M1 f) = toJSON (symbolVal $ Proxy @name, gtoRPC f)
  gfromRPC v = do
    (name, f) <- parseJSON v
    guard $ name == symbolVal (Proxy @name)
    f' <- gfromRPC f
    pure $ M1 f'

instance GShow f x => GShow (M1 _2 ('MetaData _1 _3 _4 _5) f) x where
  gtoRPC (M1 f) = gtoRPC f
  gfromRPC = fmap M1 . gfromRPC

instance GShow f x => GShow (M1 _2 ('MetaSel _1 _3 _4 _5) f) x where
  gtoRPC (M1 f) = gtoRPC f
  gfromRPC = fmap M1 . gfromRPC


data Dict c where
  Dict :: c => Dict c

instance ( Interpret ('Kon c) x => GShow f x
         ) => GShow ('Kon c :=>: f) x where
  gtoRPC (SuchThat f) = gtoRPC f
  gfromRPC v = do
    case unsafeCoerce (Dict @(Int ~ Int)) :: Dict (Interpret ('Kon c) x) of
      Dict -> do
        f <- gfromRPC v
        pure $ SuchThat f

hello :: Member TTY r => Sem r ()
hello = writeTTY "hello"


-- runEffectAsRPC
--     :: ( RPCable e
--        , Member (Lift IO) r
--        , Member RPC r
--        )
--     => endpoint
--     -> Sem (e ': r) a
--     -> Sem r a
-- runEffectAsRPC endpoint = interpretH $ \e -> do
--   liftT $ do
--     resp <- doRPC $ toRPC e
--     let Success a = fromJSON resp
--     pure a


-- main :: IO ()
-- main = runM . runEffectAsRPC @TTY () $ hello

