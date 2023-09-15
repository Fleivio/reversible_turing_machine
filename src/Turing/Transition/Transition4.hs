module Turing.Transition.Transition4 (Transition4 (..), getTransition, getLastTransition) where

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
  show (Tr4 f ia t oa) = leftSide ++ alignment ++ " -> " ++ rightSide
    where
      stateAlign = 13
      arrowAlign = 40

      alignment = align arrowAlign leftSide
      leftSide = "(" ++ show f ++ alig1 ++ ", " ++ show ia ++ ")"
        where 
          alig1 = align stateAlign (show f)
      rightSide = "(" ++ show t ++ alig2 ++ ", " ++ show oa ++ ")"
        where 
          alig2 = align stateAlign (show t)

      align :: Int -> String -> String
      align n str = replicate (n - length str) ' '

hasValidCondition :: Transition4 -> State -> TripleInAction -> Bool
hasValidCondition tr state symb3 = from tr == state && inAct tr == symb3

getTransition :: State -> TripleInAction -> [Transition4] -> Maybe Transition4
getTransition state symb3 = find (\x -> hasValidCondition x state symb3)

getLastTransition :: State -> [Transition4] -> Transition4
getLastTransition lastState trs = head $ filter (\x -> to x == lastState) trs