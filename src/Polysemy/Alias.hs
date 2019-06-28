module Polysemy.Alias where

import Polysemy

type InterpreterOf e r = forall x.  Sem (e ': r) x -> Sem r x

