-- | Operators meant as replacements for traditional 'Sem' type and 'Member' /
-- 'Members' constraints, that allow you to specify types of your actions and
-- interpreters in more concise way, without mentioning unnecessary details:
--
-- @
-- foo :: 'Member' ('Embed' 'IO') r => 'String' -> 'Int' -> 'Sem' r ()
-- @
--
-- can be written simply as:
--
-- @
-- foo :: 'String' -> 'Int' -> 'IO' '~@>' ()
-- @
--
-- Working example with operators:
--
-- @
-- import Data.Function
-- import Polysemy
-- import Polysemy.Operators
-- import Polysemy.Random
--
-- data ConsoleIO m a where
--   WriteStrLn ::           'String' -> ConsoleIO m ()
--   ReadStrLn  ::                     ConsoleIO m 'String'
--   ShowStrLn  :: 'Show' a => a      -> ConsoleIO m ()
--
-- 'makeSem' ''ConsoleIO
--
-- -- runConsoleIO :: Member (Embed IO) r => Sem (ConsoleIO : r) a -> Sem r a
-- runConsoleIO :: ConsoleIO : r '@>' a -> 'IO' '~@' r '@>' a
-- runConsoleIO = 'interpret' \\case
--   WriteStrLn s -> 'sendM' '$' 'putStrLn' s
--   ReadStrLn    -> 'sendM'   'getLine'
--   ShowStrLn  v -> 'sendM' '$' 'print' v
--
-- main :: 'IO' ()
-- main = program
--      'Data.Function.&' runConsoleIO
--      'Data.Function.&' 'Polysemy.Random.runRandomIO'
--      'Data.Function.&' 'runM'
--
-- -- program :: Members \'[Random, ConsoleIO] r => Sem r ()
-- program :: \'['Polysemy.Random', ConsoleIO] '>@>' ()
-- program = do
--   writeStrLn "It works! Write something:"
--   val <- readStrLn
--   writeStrLn '$' "Here it is!: " '++' val
--   num <- 'Polysemy.Random.random' \@'Int'
--   writeStrLn '$' "Some random number:"
--   showStrLn num
-- @
--
-- Please keep in mind that constraints created through these operators are
-- limited to the action they are being used on, for example:
--
-- @
-- foo :: (forall x. r '@>' x -> 'IO' x)
--     -> 'IO' (forall a. Foo : r '@>' a -> 'IO' '~@' r '@>' a)
-- @
--
-- The first argument in the signature above won't have access to the
-- @('IO' ~\@)@ constraint in the result - in such cases, use a normal
-- constraint instead:
--
-- @
-- foo :: 'Member' ('Embed' 'IO') r
--     => (forall x. r '@>' x -> 'IO' x)
--     -> 'IO' (forall a. Foo : r '@>' a -> r '@>' a)
-- @
--
-- See the documentation of specific operators for more details.

module Polysemy.Operators
  ( -- * 'Sem' operators
    type (@>)
  , type (@-)
  , type (@~)
    -- $SemOperators

  , -- * 'Member' operators
    type (>@)
  , type (-@)
  , type (~@)
    -- $MemberOperators

  , -- * Combined operators
    type (>@>)
  , type (-@>)
  , type (~@>)
    -- $CombinedOperators
  ) where

import Polysemy

-- Miscellaneous -------------------------------------------------------------
-- | Gets list of effects from 'Sem'.
type family SemList s where
  SemList (Sem r _) = r

-- Operators -----------------------------------------------------------------

-- $SemOperators
-- Infix equivalents of 'Sem' with versions for specifiying list of effects
-- ('@>'), single effect ('@-') and single monad ('@~') as effects of union.
-- Use ('>@>'), ('-@>') or ('~@>') instead if you are not making any
-- transformations on union and just want to use some members instead.
--
-- __Examples:__
--
-- 'Sem' with list of multiple effects:
--
-- @
-- foo :: 'Sem' ('Polysemy.State.State' 'Int' : r) ()
-- @
--
-- can be written as:
--
-- @
-- foo :: 'Polysemy.State.State' 'Int' : r '@>' ()
-- @
--
-- 'Sem' with list of one effect:
--
-- @
-- foo :: 'Sem' \'['Polysemy.State.State' 'Int'] ()
-- @
--
-- can be written as both (with the latter preferred):
--
-- @
-- foo :: \'['Polysemy.State.State' 'Int'] '@>' ()
-- @
--
-- and:
--
-- @
-- foo :: 'Polysemy.State.State' 'Int' '@-' ()
-- @
--
-- where effect without list gets put into one automatically.
--
-- 'Sem' with __exactly__ one, lifted monad:
--
-- @
-- foo :: 'Sem' \'['Embed' 'IO'] ()
-- @
--
-- can be written simply as:
--
-- @
-- foo :: 'IO' '@~' ()
-- @
--
-- and will be automatically lifted and put into list.
infix 2 @>, @-, @~

type (@>)   = Sem
type (@-) e = Sem '[e]
type (@~) m = Sem '[Embed m]

-- $MemberOperators
-- Infix equivalents of 'Member'(s) constraint used directly in /return/ type,
-- specifiying list of members ('>@'), single member ('-@') or single monad
-- ('~@'), meant to be paired with some of the 'Sem' operators (('@>'), ('@-')
-- and ('@~')). Use ('>@>'), ('-@>') or ('~@>') instead if you are not making
-- any transformations on union and just want to use some members instead.
--
-- __Examples:__
--
-- List of multiple members:
--
-- @
-- foo :: 'Members' \'['Polysemy.State.State' 'Int', 'Polysemy.Input.Input' 'String'] r => 'Sem' ('Polysemy.Output.Output' ['String'] : r) () -> 'Sem' r ()
-- @
--
-- can be written as:
--
-- @
-- foo :: 'Polysemy.Output.Output' ['String'] : r '@>' () -> \'['Polysemy.State.State' 'Int', 'Polysemy.Input.Input' 'String'] '>@' r '@>' ()
-- @
--
-- One member:
--
-- @
-- foo :: 'Member' ('Polysemy.State.State' 'Int') r => 'Sem' ('Polysemy.Output.Output' ['String'] : r) () -> 'Sem' r ()
-- @
--
-- can be written as both (with the latter preferred):
--
-- @
-- foo :: 'Polysemy.Output.Output' ['String'] : r '@>' () -> \'['Polysemy.State.State' 'Int'] '>@' r '@>' ()
-- @
--
-- and:
--
-- @
-- foo :: 'Polysemy.Output.Output' ['String'] : r '@>' () -> 'Polysemy.State.State' 'Int' '-@' r '@>' ()
-- @
--
-- __Exactly__ one, lifted monad as a member:
--
-- @
-- foo :: 'Member' ('Embed' 'IO') r => 'Sem' ('Polysemy.Output.Output' ['String'] : r) () -> 'Sem' r ()
-- @
--
-- can be written simply as:
--
-- @
-- foo :: 'Polysemy.Output.Output' ['String'] : r '@>' () -> 'IO' '~@' r '@>' ()
-- @
infix 1 >@, -@, ~@

type (>@) es s = Members es       (SemList s) => s
type (-@) e  s = Member  e        (SemList s) => s
type (~@) m  s = Member  (Embed m) (SemList s) => s

-- $CombinedOperators
-- Joined versions of one of ('>@'), ('-@'), ('~@') and ('@>') with implicit,
-- hidden list of effects in union --- suited for actions that only use one
-- 'Sem' in their type.
--
-- __Examples:__
--
-- List of members over some 'Sem':
--
-- @
-- foo :: 'Members' \'['Polysemy.State.State' 'String', 'Polysemy.Input.Input' 'Int'] r => 'String' -> 'Int' -> 'Sem' r ()
-- @
--
-- can be written as:
--
-- @
-- foo :: 'String' -> 'Int' -> \'['Polysemy.State.State' 'String', 'Polysemy.Input.Input' 'Int'] '>@>' ()
-- @
--
-- Single member:
--
-- @
-- foo :: 'Member' ('Polysemy.Input.Input' 'Int') r => 'String' -> 'Int' -> 'Sem' r ()
-- @
--
-- can be written as both (with the latter preferred):
--
-- @
-- foo :: 'String' -> 'Int' -> \'['Polysemy.Input.Input' 'Int'] '>@>' ()
-- @
--
-- and:
--
-- @
-- foo :: 'String' -> 'Int' -> 'Polysemy.Input.Input' 'Int' '-@>' ()
-- @
--
-- __Exactly__ one, lifted monad as a member:
--
-- @
-- foo :: 'Member' ('Embed' 'IO') r => 'Sem' r ()
-- @
--
-- can be written simply as:
--
-- @
-- foo :: 'IO' '~@>' ()
-- @
infix 1 >@>, -@>, ~@>

type (>@>) es a = forall r. Members es       r => Sem r a
type (-@>) e  a = forall r. Member  e        r => Sem r a
type (~@>) m  a = forall r. Member  (Embed m) r => Sem r a
