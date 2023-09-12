module Turing.Transition.Transition4 (Transition4 (..), getTransition) where

import Data.List (find)
import Turing.Basic.State
import Turing.Tape.TripleTape

data Transition4 = Tr4
  { from :: State,
    inAct :: TripleInAction,
    to :: State,
    outAct :: TripleOutAction
  }
  deriving (Eq)

instance Show Transition4 where
  show (Tr4 f ia t oa) =
    "(" ++ show f ++ ", " ++ show ia ++ ") -> (" ++ show t ++ ", " ++ show oa ++ ")"

hasValidCondition :: Transition4 -> State -> TripleInAction -> Bool
hasValidCondition tr state symb3 = from tr == state && inAct tr == symb3

getTransition :: State -> TripleInAction -> [Transition4] -> Maybe Transition4
getTransition state symb3 = find (\x -> hasValidCondition x state symb3)