module Turing.Transition.Transition4 (Transition4 (..), getTransition, getLastTransition, reverseQuadruple) where

import Data.List (find)
import Turing.Basic.State
import Turing.Tape.TripleTape
import Turing.Basic.Action
import Turing.Basic.Direction

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

reverseQuadruple :: Transition4 -> Transition4
reverseQuadruple
  Tr4
    { from = stFrom,
      inAct = inAction,
      to = stTo,
      outAct = outAction
    } = case (inAction, outAction) of
      ((Readt r1, Bar, _), (Writet w1, Shift R, _))
        -> Tr4
          { from = inverseState stTo,
            inAct = (Readt w1, Bar, readEmpty),
            to = inverseState stFrom,
            outAct = (Writet r1, Shift L, writeEmpty)
          }
      ((Bar, _, Bar), (Shift dir, Writet interS, Shift S))
        -> Tr4
            { from = inverseState stTo,
              inAct = (Bar, Readt interS, Bar),
              to = inverseState stFrom,
              outAct = (Shift (revDir dir), writeEmpty, Shift S)
            }
      _ -> error "Invalid quadruple to reverse"
