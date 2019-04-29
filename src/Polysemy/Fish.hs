{-# LANGUAGE KindSignatures, UndecidableInstances, ConstraintKinds #-}

module Polysemy.Fish
  ( -- $FishOperators
    type (>@)
  , type (@>)
  , type (>@>)
  , -- * Miscellaneous
    Means
  , module Polysemy
  ) where

import Data.Kind
import GHC.TypeLits
import Polysemy

-- $FishOperators
-- (__NOTE:__ This module is experimental and may change it's API in the
-- future --- but experimenting with it and providing feedback is highly
-- appreciated!)
--
-- Operators meant as replacements for traditional 'Sem' type and 'Member' /
-- 'Members' constraints, that allow you to specify types of your actions and
-- interpreters in more concise way, e.g. without mentioning unnecessary
-- details:
--
-- @
-- foo :: 'Member' ('Lift' 'IO') r => 'String' -> 'Int' -> 'Sem' r ()
-- @
--
-- can be written simply as:
--
-- @
-- foo :: 'String' -> 'Int' -> 'IO' >\@> ()
-- @
--
-- See documentation of specific operators for more details.


------------------------------------------------------------------------------
-- Compile-time errors -------------------------------------------------------
------------------------------------------------------------------------------
-- | Error to call when supplied effect(s) are missing types of monad and
-- value in their declaration. Used by 'ToEffList' which requires them to
-- fully reduce during typechecking.
type family MissingEffTypes (caller :: Symbol) (es :: k) :: a where
  MissingEffTypes c (e ::  k -> l -> m)      = MissingEffTypes c (
      Text "Effect '" :<>: ShowType e :<>: Text "' is"
    )
  MissingEffTypes c (es :: [k -> l -> m])    = MissingEffTypes c (
      Text "Effects in list '" :<>: ShowType es :<>: Text "' are"
    )
  -- Part shared by cases of both one and multiple effects
  MissingEffTypes c (prefix :: ErrorMessage) = TypeError (
         prefix :<>: Text " missing concrete monad and value types"
    :$$: Text "  needed by '" :<>: Text c :<>: Text "'"
    :$$: Text "Specify them in effect's GADT declaration:"
    :$$: Text "  data <Effect> <custom arguments..> (m :: * -> *) (a :: *) where ..."
    :$$: Text "                                     ^^^^^^^^^^^^^ ^^^^^^^^"
    :$$: Text "Hint: if you can't change declaration of specific list,"
    :$$: Text "  use type annotation '(* -> *) -> * -> *' or synonym 'Eff' from 'Polysemy.Fish'"
    )

-- | Error to call when user supplies list type instead of type-level list
-- literal.
type family MissingListQuote (type_ :: k) :: a where
  MissingListQuote t = TypeError (
         Text "Unexpected list type: " :<>: ShowType t
    :$$: Text "Perhaps you meant a type-level list? (with quote): '" :<>: ShowType t
    )

-- | Called by 'ToEffList' when supplied type is not convertible into list of
-- effects.
type family ExpectedEffListMonadFound (x :: k) :: a where
  ExpectedEffListMonadFound x = TypeError (
      Text "Expected effect, list of effects or monad, found '" :<>: ShowType x :<>: Text "'"
    )


------------------------------------------------------------------------------
-- Effect list construction --------------------------------------------------
------------------------------------------------------------------------------
-- | Converts value into list of effects to be used by other combinators.
type family ToEffList (caller :: Symbol)
                      (es :: k)
                   :: [(Type -> Type) -> Type -> Type] where
  -- Single effect to wrap in list -------------------------------------------
  ToEffList _ (e :: (Type -> Type) -> Type -> Type)    = '[e]
  -- We need concrete types to get GHC to reduce our type family
  ToEffList c (e :: k -> l -> Type)                    = MissingEffTypes c e
  -- List of effects ---------------------------------------------------------
  -- Empty case to help type inference
  ToEffList _ '[]                                      = '[]
  ToEffList _ (es :: [(Type -> Type) -> Type -> Type]) = es
  ToEffList c (es :: [k -> l -> Type])                 = MissingEffTypes c es
  -- We do not want to pretend list type is valid value --- instead we throw
  -- an error
  ToEffList _ []                                       = MissingListQuote []
  ToEffList _ [t]                                      = MissingListQuote [t]
  -- Single 'Monad' to wrap in 'Lift' and list -------------------------------
  ToEffList _ (m :: Type -> Type)                      = '[Lift m]
  -- Unexcpected value -------------------------------------------------------
  ToEffList _ (x :: k)                                 = ExpectedEffListMonadFound x

-- | Extracts list of effects from 'Sem'.
type family SemList s where
  SemList (Sem r _) = r


------------------------------------------------------------------------------
-- "Fish" operators ----------------------------------------------------------
------------------------------------------------------------------------------
-- | Infix equivalent of 'Sem' with automatic conversion of supplied effects
-- into list. Use ('>@>') instead if you only want to work with 'Members' of
-- your union.
--
-- __Examples:__
--
-- 'Sem' with list of multiple effects:
--
-- @
-- foo :: 'Sem' \'['Polysemy.State.State' 'Int', 'Polysemy.Input.Input' 'String'] ()
-- @
--
-- can be written as:
--
-- @
-- foo :: \'['Polysemy.State.State' 'Int', 'Polysemy.Input.Input' 'String'] \@> ()
-- @
--
-- 'Sem' with list of one effect:
--
-- @
-- foo :: 'Sem' \'['Polysemy.State.State' 'Int'] ()
-- @
--
-- can be written as both:
--
-- @
-- foo :: \'['Polysemy.State.State' 'Int'] \@> ()
-- @
--
-- and:
--
-- @
-- foo :: 'Polysemy.State.State' 'Int' \@> ()
-- @
--
-- where effect without list gets put into one automatically.
--
-- 'Sem' with __exactly__ one, lifted monad:
--
-- @
-- foo :: 'Sem' \'['Lift' 'IO'] ()
-- @
--
-- can be written simply as:
--
-- @
-- foo :: 'IO' \@> ()
-- @
--
-- and will be automatically lifted and put into list.
--
-- __NOTE:__ For this operator to work, you have to provide types of monad and
-- contained value in your effect:
--
-- @
-- data MyEffect x y (m :: * -> *) (a :: *) where ...
-- @
--
-- but this requirement may be removed in the future.
infix 2 @>
type es @> r = Sem (ToEffList "(@>)" es) r

-- | Infix equivalent of 'Members' constraint used directly in /return/ type,
-- with automatic conversion of supplied effects into list. Can be used in
-- combination with ('@>'). Use ('>@>') instead if you only want to work with
-- 'Members' of your union.
--
-- __Examples:__
--
-- List of multiple members:
--
-- @
-- foo :: 'Members' \'['Polysemy.State.State' 'Int', 'Polysemy.Input.Input' 'String'] r => 'Sem' (ConsoleIO:r) () -> 'Sem' r ()
-- @
--
-- can be written as:
--
-- @
-- foo :: ConsoleIO:r \@> () -> \'['Polysemy.State.State' 'Int', 'Polysemy.Input.Input' 'String'] >\@ r \@> ()
-- @
--
-- One member:
--
-- @
-- foo :: 'Member' ('Polysemy.State.State' 'Int') r => 'Sem' (ConsoleIO:r) () -> 'Sem' r ()
-- @
--
-- can be written as both:
--
-- @
-- foo :: ConsoleIO:r \@> () -> \'['Polysemy.State.State' 'Int'] >\@ r \@> ()
-- @
--
-- and:
--
-- @
-- foo :: ConsoleIO:r \@> () -> 'Polysemy.State.State' 'Int' >\@ r \@> ()
-- @
--
-- where effect without list gets put into one automatically.
--
-- __Exactly__ one, lifted monad as a member:
--
-- @
-- foo :: 'Member' ('Lift' 'IO') r => 'Sem' (ConsoleIO:r) () -> 'Sem' r ()
-- @
--
-- can be written simply as:
--
-- @
-- foo :: ConsoleIO:r \@> () -> 'IO' >\@ r \@> ()
-- @
--
-- and will be automatically lifted and put into list.
--
-- __NOTE:__ For this operator to work, you have to provide types of monad and
-- contained value in your effect:
--
-- @
-- data MyEffect x y (m :: * -> *) (a :: *) where ...
-- @
--
-- but this requirement may be removed in the future.
infix 1 >@
type es >@ s = Members (ToEffList "(>@)" es) (SemList s) => s

-- | Joined version of ('>@') and ('@>') with implicit, hidden list of effects
-- in union --- suited for actions that only use one 'Sem' in their type.
--
-- E.g.:
--
-- @
-- foo :: 'Members' \'['Polysemy.State.State' 'String', 'Polysemy.Input.Input' 'Int'] r => 'String' -> 'Int' -> 'Sem' r ()
-- @
--
-- can be written as:
--
-- @
-- foo :: 'String' -> 'Int' -> \'['Polysemy.State.State' 'String', 'Polysemy.Input.Input' 'Int'] >\@> ()
-- @
--
-- For more examples on how to specify member effects, see ('>@').
--
-- __NOTE:__ For this operator to work, you have to provide types of monad and
-- contained value in your effect:
--
-- @
-- data MyEffect x y (m :: * -> *) (a :: *) where ...
-- @
--
-- but this requirement may be removed in the future.
infix 1 >@>
type es >@> a = forall r. Members (ToEffList "(>@>)" es) r => Sem r a


------------------------------------------------------------------------------
-- Miscellaneous -------------------------------------------------------------
------------------------------------------------------------------------------
-- | Constraint version of ('>@').
--
-- __NOTE:__ For this constraint to work, you have to provide types of monad
-- and contained value in your effect:
--
-- @
-- data MyEffect x y (m :: * -> *) (a :: *) where ...
-- @
--
-- but this requirement may be removed in the future.
type Means es r = Members (ToEffList "Means" es) r
