module Turing.Action(Direction(..), OutAction(..), InAction(..), TripleInAction, TripleOutAction) where

import Turing.Symbol

data Direction = R | L | S deriving (Eq, Show)

data OutAction = Write Symbol | Shift Direction deriving (Eq, Show)

data InAction = Read Symbol | Bar deriving Show

instance Eq InAction where
    (Read s1) == (Read s2) = s1 == s2
    Bar == _ = True
    _ == Bar = True

type TripleInAction = (InAction, InAction, InAction)
type TripleOutAction = (OutAction, OutAction, OutAction)
