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

import GHC.Types
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
import Data.Type.Equality (type (==), type (~~))
import Data.Type.Bool


data Foo (b :: Type) (a :: Type) where
  Bar :: Int -> Foo Int Int
  Baz :: Foo Bool Bool

deriving instance Show (Foo b a)
deriveGenericK ''Foo

class GRead f p where
  gshow :: f p -> Value
  gread :: Value -> Parser (f p)

data Dict c where
  Dict :: c => Dict c

class Dict1 c f where
  dict1 :: f x -> Dict (c x)

instance (c Int, c Bool) => Dict1 c (Foo b) where
  dict1 (Bar _) = Dict
  dict1 Baz     = Dict

instance (GRead f x, KnownSymbol name) => GRead (M1 _2 ('MetaCons name _1 _3) f) x where
  gshow (M1 f) = toJSON (symbolVal $ Proxy @name, gshow f)
  gread v = do
    (name, f) <- parseJSON v
    guard $ name == symbolVal (Proxy @name)
    f' <- gread f
    pure $ M1 f'

instance GRead f x => GRead (M1 _2 ('MetaData _1 _3 _4 _5) f) x where
  gshow (M1 f) = gshow f
  gread = fmap M1 . gread

instance GRead f x => GRead (M1 _2 ('MetaSel _1 _3 _4 _5) f) x where
  gshow (M1 f) = gshow f
  gread = fmap M1 . gread

instance (ToJSON (Interpret t x), FromJSON (Interpret t x)) => GRead (Field t) x where
  gshow (Field f) = toJSON f
  gread = fmap Field . parseJSON

instance (GRead f p, GRead g p) => GRead (f :*: g) p where
  gshow (f :*: g) = toJSON (gshow f, gshow g)
  gread v = do
    (f, g) <- parseJSON v
    f' <- gread f
    g' <- gread g
    pure (f' :*: g')

instance (GRead f p, GRead g p) => GRead (f :+: g) p where
  gshow (L1 f) = gshow f
  gshow (R1 f) = gshow f
  gread s = fmap L1 (gread s) <|> fmap R1 (gread s)

instance GRead U1 p where
  gshow U1 = Null
  gread Null = pure U1
  gread _    = empty

instance GReadCon (Sat c) c f p => GRead (c :=>: f) p where
  gshow = gshowCon @(Sat c)
  gread = greadCon @(Sat c)

class GReadCon (b :: Bool) c f p where
  gshowCon :: (c :=>: f) p -> Value
  greadCon :: Value -> Parser ((c :=>: f) p)

instance (Interpret c p, GRead f p) => GReadCon 'True c f p where
  gshowCon (SuchThat f) = gshow f
  greadCon = fmap SuchThat . gread

instance GReadCon 'False c f p where
  gshowCon (SuchThat f) = error "impossible"
  greadCon _ = empty

-- Whether a constraint is satisfiable
-- Assumed to be a conjunction of type equalities
type family Sat (c :: Atom Type Constraint) :: Bool where
  Sat (c1 ':&: c2)    = Sat c1 && Sat c2
  Sat ('Kon (a ~~ b)) = a == b
  {- if you use (~) instead of (~~) here you get a "No instance" error
     that seems to say that, paradoxically, (Sat ('Kon (Int ~ Int))) doesn't
     reduce, but in spite of what's printed that's actually (~~).
   -}

gfromRPC :: forall a. (GenericK a LoT0, GRead (RepK a) LoT0) => Value -> Result a
gfromRPC = parse (fmap toK . gread @(RepK a) @LoT0)

gtoRPC :: forall a. (GenericK a LoT0, GRead (RepK a) LoT0) => a -> Value
gtoRPC = gshow @(RepK a) @LoT0 . fromK

readFooInt :: Value -> Result (Foo Int Int)
readFooInt = gfromRPC

readFooBool :: Value -> Result (Foo Bool Bool)
readFooBool = gfromRPC




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
  fromRPC :: Value -> Result (e m x)


instance RPCable TTY where
  toRPC = gtoRPC
    -- case x of
    --   WriteTTY _ -> gtoRPC x
    --   ReadTTY -> gtoRPC x



-- hello :: Member TTY r => Sem r ()
-- hello = writeTTY "hello"


-- -- runEffectAsRPC
-- --     :: ( RPCable e
-- --        , Member (Lift IO) r
-- --        , Member RPC r
-- --        )
-- --     => endpoint
-- --     -> Sem (e ': r) a
-- --     -> Sem r a
-- -- runEffectAsRPC endpoint = interpretH $ \e -> do
-- --   liftT $ do
-- --     resp <- doRPC $ toRPC e
-- --     let Success a = fromJSON resp
-- --     pure a


-- -- main :: IO ()
-- -- main = runM . runEffectAsRPC @TTY () $ hello

