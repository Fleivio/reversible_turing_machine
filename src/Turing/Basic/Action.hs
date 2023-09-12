module Turing.Basic.Action (OutAction (..), InAction (..)) where

import Turing.Basic.Direction (Direction)
import Turing.Basic.Symbol (Symbol)

data OutAction = Writet Symbol | Shift Direction deriving Eq

data InAction = Readt Symbol | Bar

instance Show InAction where
  show (Readt s) = "'" ++ s ++ "'"
  show Bar = "*"

instance Show OutAction where
  show (Writet s) = "'" ++ s ++ "'"
  show (Shift d) = show d

instance Eq InAction where
  (Readt s1) == (Readt s2) = s1 == s2
  Bar == _ = True
  _ == Bar = True