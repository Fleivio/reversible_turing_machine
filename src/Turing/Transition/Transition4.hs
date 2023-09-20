{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Turing.Transition.Transition4 (Transition4 (..), Transition(..), reverseQuadruple) where

import Turing.Transition.Transition
import Turing.Basic.State
import Turing.Tape.TripleTape
import Turing.Basic.Action
import Turing.Basic.Direction

data Transition4 = Tr4
  { from4 :: State,
    inAct :: TripleInAction,
    to4 :: State,
    outAct :: TripleOutAction
  }
  deriving (Eq)

instance Show Transition4 where
  show (Tr4 f ia t oa) 
   = "(" ++ show f ++ "," ++ show ia ++ ") -> (" ++ show t ++ "," ++ show oa ++ ")"
    
instance Transition Transition4 TripleInAction where
  hasValidCondition (Tr4 f ia _ _) state symb3 = f == state && ia == symb3

  from = from4
  to = to4

reverseQuadruple :: Transition4 -> Transition4
reverseQuadruple
  Tr4
    { from4  = stFrom,
      inAct  = inAction,
      to4    = stTo,
      outAct = outAction
    } = case (inAction, outAction) of
      ((Readt r1, Bar, _), (Writet w1, Shift R, _))
        -> Tr4
          { from4   = inverseState stTo,
            inAct  = (Readt w1, Bar, readEmpty),
            to4     = inverseState stFrom,
            outAct = (Writet r1, Shift L, writeEmpty)
          }
      ((Bar, _, Bar), (Shift dir, Writet interS, Shift S))
        -> Tr4
            { from4   = inverseState stTo,
              inAct  = (Bar, Readt interS, Bar),
              to4     = inverseState stFrom,
              outAct = (Shift (revDir dir), writeEmpty, Shift S)
            }
      _ -> error "Invalid quadruple to reverse"
