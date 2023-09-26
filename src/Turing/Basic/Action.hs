module Turing.Basic.Action (OutAction (..), Symbol, Direction(..), InAction (..), readEmpty, writeEmpty, noShift) where

import Turing.Basic.Direction (Direction (..))
import Turing.Basic.Symbol (Symbol, emptySymb)

data OutAction
  = Wrt Symbol
  | Sft Direction
  deriving (Eq)

data InAction
  = Rd Symbol
  | Bar

instance Eq InAction where
  (Rd s1) == (Rd s2) = s1 == s2
  Bar == _ = True
  _ == Bar = True

readEmpty :: InAction
readEmpty = Rd emptySymb

writeEmpty :: OutAction
writeEmpty = Wrt emptySymb

noShift :: OutAction
noShift = Sft Z

instance Show InAction where
  show (Rd s) = s
  show Bar = "/"

instance Show OutAction where
  show (Wrt s) = s
  show (Sft d) = show d