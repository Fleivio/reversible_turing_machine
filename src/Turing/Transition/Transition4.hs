module Turing.Transition.Transition4 (Transition4 (..), getTransition) where

import Turing.Basic.State
import Turing.Tape.TripleTape

data Transition4 = Transition4
  { from :: State,
    inAct :: TripleInAction,
    to :: State,
    outAct :: TripleOutAction
  }
  deriving (Eq)

instance Show Transition4 where
  show (Transition4 f ia t oa) =
    "(" ++ show f ++ ", " ++ show ia ++ ") -> (" ++ show t ++ ", " ++ show oa ++ ")"

hasValidCondition :: Transition4 -> State -> TripleInAction -> Bool
hasValidCondition q s tia = from q == s && inAct q == tia

getTransition :: State -> TripleInAction -> [Transition4] -> Maybe Transition4
getTransition state tia trs =
  if null validCases
    then Nothing
    else Just $ head validCases
  where
    validCases = filter (\z -> hasValidCondition z state tia) trs