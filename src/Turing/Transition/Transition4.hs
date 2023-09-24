{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Turing.Transition.Transition4 (Transition4 (..), Transition (..), reverseQuadruple) where

import Turing.Basic.Action
import Turing.Basic.Direction
import Turing.Basic.State
import Turing.Tape.TripleTape
import Turing.Transition.Transition

data Transition4 = Tr4
  { from4 :: State,
    inAct :: TripleInAction,
    to4 :: State,
    outAct :: TripleOutAction
  }
  deriving (Eq)

instance Show Transition4 where
  show (Tr4 f ia t oa) =
    "(" ++ f ++ "," ++ show ia ++ ") -> (" ++ t ++ "," ++ show oa ++ ")"

instance Transition Transition4 TripleInAction where
  hasValidCondition (Tr4 f ia _ _) state symb3 = f == state && ia == symb3

  from = from4
  to = to4

reverseQuadruple :: Transition4 -> Transition4
reverseQuadruple
  Tr4
    { from4 = stFrom,
      inAct = inAction,
      to4 = stTo,
      outAct = outAction
    } =
    (\(a1, a2) -> Tr4 invTo a1 invFrom a2) $
      case (inAction, outAction) of
        ((Readt r1, Bar, _), (Writet w1, Shift R, _)) -> (,)
          (Readt w1, Bar, readEmpty)
          (Writet r1, Shift L, writeEmpty)
        ((Bar, _, Bar), (Shift dir, Writet interS, Shift Z)) -> (,)
          (Bar, Readt interS, Bar)
          (Shift (revDir dir), writeEmpty, Shift Z)
          
        _ -> error "Invalid quadruple to reverse"
    where
      invFrom = inverseState stFrom
      invTo = inverseState stTo
