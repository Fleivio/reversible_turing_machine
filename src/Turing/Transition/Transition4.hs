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
  (Tr4 stFrom inAction stTo outAction)
   = Tr4 invTo invIn invFrom invOut
    where
      (invIn, invOut)
       = case (inAction, outAction) of
          ((Rd r1, Bar, _), (Wrt w1, Sft R, _)) -> (,)
            (Rd w1 , Bar  , readEmpty)
            (Wrt r1, Sft L, writeEmpty)
          ((Bar, _, Bar), (Sft dir, Wrt interS, Sft Z)) -> (,)
            (Bar, Rd interS , Bar)
            (Sft (revDir dir), writeEmpty, Sft Z)
          _ -> error "Invalid quadruple to reverse"
      invFrom = inverseState stFrom
      invTo = inverseState stTo
