module Turing.Tape.Basic.Action(OutAction(..), InAction(..)) where

import Turing.Tape.Basic.Direction ( Direction )

data OutAction a = Writet a | Shift Direction deriving (Eq, Show)

data InAction a = Readt a | Bar deriving (Show)

instance (Eq a) => Eq (InAction a) where
    (Readt s1) == (Readt s2) = s1 == s2
    Bar == _ = True
    _ == Bar = True