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

import Data.Typeable
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Kind
import Data.Proxy
import GHC.Generics (Meta (..))
import GHC.TypeLits
import GHC.Types
import Generics.Kind
import Generics.Kind.TH
import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Tactics
import Unsafe.Coerce


data RPC m a where
  DoRPC :: Value -> RPC m Value

makeSem ''RPC


data TTY (m :: * -> *) a where
  WriteTTY :: String -> TTY m ()
  ReadTTY :: TTY m String

deriving instance Show (TTY m a)

type family Froom f x :: [Type] where
  Froom U1 x           = '[]
  Froom V1 x           = '[]
  Froom (Field _1) x   = '[]
  Froom (M1 _1 _2 f) x = Froom f x
  Froom (cs :=>: f) x  = GetFroom cs x ++ Froom f x
  Froom (f :+: g) x    = Froom f x ++ Froom g x
  Froom (f :*: g) x    = Froom f x ++ Froom g x

type family GetFroom c x :: [Type] where
  GetFroom (Kon (x ~~ b)) x = '[b]
  GetFroom (Kon _1) x       = '[]
  GetFroom (c1 :&: c2) x = GetFroom c1 x ++ GetFroom c2 x

type family MakeConstraints
      (c :: Type -> Constraint)
      (base :: Type -> Type)
      (ts :: [Type]) :: Constraint where
  MakeConstraints c base '[] = ()
  MakeConstraints c base (t ': ts) = (c t, c (base t), MakeConstraints c base ts)

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)



deriveGenericK ''TTY
makeSem ''TTY

instance ToJSON (TTY m x) where
  toJSON (WriteTTY msg) = toJSON ("WriteTTY", msg)
  toJSON ReadTTY        = toJSON ("ReadTTY", Null)

instance FromJSON (TTY m ()) where
  parseJSON v = do
    (field, d) <- parseJSON v
    case field of
      "WriteTTY" -> do
        msg <- parseJSON d
        pure $ WriteTTY msg
      _ -> empty

instance FromJSON (TTY m String) where
  parseJSON v = do
    (field, Null) <- parseJSON v
    case field of
      "ReadTTY" -> do
        pure ReadTTY
      _ -> empty

type RPCable c e = forall (m :: * -> *) x. MakeConstraints c (e m) (Froom (RepK (e m x)) x)

data Dict c where
  Dict :: c => Dict c

class Dict1 c (e :: (* -> *) -> * -> *) where
  dict1 :: e m x -> Dict (c (e m x))
  -- dict2 :: e m x -> Dict (c x)

-- instance (RPCable c TTY) => Dict1 c TTY where
--   dict1 = \case
--     ReadTTY -> Dict
--     -- WriteTTY _ -> Dict


-- runViaRPC
--   :: ( Member RPC r
--      , Dict1 ToJSON e
--      , Dict1 FromJSON e
--      )
--   => Sem (e ': r) a
--   -> Sem r a
-- runViaRPC = interpretH $ \case
--   e -> liftT $ do
--     case (dict1 @ToJSON e, dict2 @FromJSON e) of
--       (Dict, Dict) -> do
--         r <- doRPC $ toJSON e
--         let Success x = fromJSON r
--         pure x


-- -- TODO(sandy): do something with errors here
-- eliminateRPC :: Sem (RPC ': r) a -> Sem r a
-- eliminateRPC = interpret $ \case
--   DoRPC m -> pure m

-- -- withEffect :: Value -> (forall m x. e m x

-- -- handleRPC
-- --     :: forall e r a
-- --      . ( Member e r
-- --        , Member RPC r
-- --        , forall m. Dict1 ToJSON (e m)
-- --        )
-- --     => Sem r a
-- --     -> Sem r a
-- -- handleRPC = intercept $ \case
-- --   DoRPC msg -> do
-- --     let Success x = fromJSON msg
-- --     r <- send @e x
-- --     pure $ toJSON r





-- -- runEffectAsRPC
-- --     :: ( RPCable e
-- --        , forall m. Dict1 FromJSON (e m)
-- --        , Member (Lift IO) r
-- --        , Member RPC r
-- --        )
-- --     => endpoint
-- --     -> Sem (e ': r) a
-- --     -> Sem r a
-- -- runEffectAsRPC endpoint = interpretH $ \e -> do
-- --   liftT $ do
-- --     resp <- doRPC $ toJSON e
-- --     case dict1 @FromJSON e of
-- --       Dict -> do
-- --         let Success a = fromJSON resp
-- --         pure a


-- -- -- main :: IO ()
-- -- -- main = runM . runEffectAsRPC @TTY () $ hello

