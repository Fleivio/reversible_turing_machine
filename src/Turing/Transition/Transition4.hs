module Turing.Transition.Transition4 (Transition4 (..), getTransition, reverseQuadruple, getTransitionThatGoesTo) where

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
  show (Tr4 f ia t oa) 
   = "(" ++ show f ++ ", " ++ show ia ++ ") -> (" ++ show t ++ ", " ++ show oa ++ ")"
    

hasValidCondition :: Transition4 -> State -> TripleInAction -> Bool
hasValidCondition tr state symb3 = from tr == state && inAct tr == symb3

getTransition :: State -> TripleInAction -> [Transition4] -> Maybe Transition4
getTransition state symb3 = find (\x -> hasValidCondition x state symb3)

getTransitionThatGoesTo :: State -> [Transition4] -> Transition4
getTransitionThatGoesTo state trs = head $ filter (\x -> to x == state) trs

reverseQuadruple :: Transition4 -> Transition4
reverseQuadruple
  Tr4
    { from   = stFrom,
      inAct  = inAction,
      to     = stTo,
      outAct = outAction
    } = case (inAction, outAction) of
      ((Readt r1, Bar, _), (Writet w1, Shift R, _))
        -> Tr4
          { from   = inverseState stTo,
            inAct  = (Readt w1, Bar, readEmpty),
            to     = inverseState stFrom,
            outAct = (Writet r1, Shift L, writeEmpty)
          }
      ((Bar, _, Bar), (Shift dir, Writet interS, Shift S))
        -> Tr4
            { from   = inverseState stTo,
              inAct  = (Bar, Readt interS, Bar),
              to     = inverseState stFrom,
              outAct = (Shift (revDir dir), writeEmpty, Shift S)
            }
      _ -> error "Invalid quadruple to reverse"
