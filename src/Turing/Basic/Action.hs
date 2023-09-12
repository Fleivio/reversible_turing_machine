module Turing.Basic.Action (OutAction (..), InAction (..)) where

import Turing.Basic.Direction (Direction)
import Turing.Basic.Symbol (Symbol)

data OutAction = Writet Symbol | Shift Direction deriving (Eq, Show)

data InAction = Readt Symbol | Bar deriving (Show)

instance Eq InAction where
  (Readt s1) == (Readt s2) = s1 == s2
  Bar == _ = True
  _ == Bar = True