module Turing.Basic.Action (OutAction (..), InAction (..), readEmpty, writeEmpty, noShift) where

import Turing.Basic.Direction (Direction(..))
import Turing.Basic.Symbol (Symbol, emptySymb)

data OutAction
  = Writet Symbol
  | Shift Direction
  deriving (Eq)

data InAction
  = Readt Symbol
  | Bar

readEmpty :: InAction
readEmpty = Readt emptySymb

writeEmpty :: OutAction
writeEmpty = Writet emptySymb

noShift :: OutAction
noShift = Shift S

instance Show InAction where
  show (Readt s) = "'" ++ s ++ "'"
  show Bar = "/"

instance Show OutAction where
  show (Writet s) = "'" ++ s ++ "'"
  show (Shift d) = show d

instance Eq InAction where
  (Readt s1) == (Readt s2) = s1 == s2
  Bar == _ = True
  _ == Bar = True