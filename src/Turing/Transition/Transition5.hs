module Turing.Transition.Transition5 (Transition5 (..), hasValidCondition, getTransition) where

import Data.List (find)
import Turing.Basic.Direction (Direction)
import Turing.Basic.State
import Turing.Basic.Symbol

data Transition5 = Tr5
  { from :: State,
    rSym :: Symbol,
    to :: State,
    wSym :: Symbol,
    dir :: Direction
  }
  deriving (Eq)

instance Show Transition5 where
  show (Tr5 f rs t ws d) = leftSide ++ alignment ++ " -> " ++ rightSide
    where
      alignment = replicate (8 - length leftSide) ' '
      leftSide  = "(" ++ show f ++ ", " ++ show rs ++ ")"
      rightSide = "(" ++ show t ++ ", " ++ show ws ++ ", " ++ show d ++ ")"

hasValidCondition :: Transition5 -> State -> Symbol -> Bool
hasValidCondition tr state symb = from tr == state && rSym tr == symb

getTransition :: State -> Symbol -> [Transition5] -> Maybe Transition5
getTransition state symb = find (\x -> hasValidCondition x state symb)