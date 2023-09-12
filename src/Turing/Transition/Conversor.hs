{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}
module Turing.Transition.Conversor (toQuadruple, genComputeTransitions, genOutputCopyTransitions) where

import Turing.Basic.Action
import Turing.Basic.Direction
import Turing.Basic.State
import Turing.Basic.Symbol
import Turing.Transition.Transition4
import qualified Turing.Transition.Transition5 as T5

toQuadruple :: T5.Transition5 -> (Transition4, Transition4)
toQuadruple q = (first, second)
  where
    interState = stCombine (T5.from q) (T5.to q) (T5.rSym q)
    first =
      Tr4
        { from = T5.from q,
          inAct = (Readt (T5.rSym q), Bar, Readt emptySymb),
          to = interState,
          outAct = (Writet (T5.wSym q), Shift R, Writet emptySymb)
        }
    second =
      Tr4
        { from = interState,
          inAct = (Bar, Readt emptySymb, Bar),
          to = T5.to q,
          outAct = (Shift (T5.dir q), Writet (stGetName interState), Shift S)
        }

genComputeTransitions :: [T5.Transition5] -> [Transition4]
genComputeTransitions trs5 = concatMap (\(x, y) -> [x, y]) tuples
  where
    tuples = map toQuadruple trs5

genOutputCopyTransitions :: State -> [Symbol] -> ([Transition4], State)
genOutputCopyTransitions af alphabet =
  ([afb1l, b1lb1, b1b2l, b2lb2, b2cf] ++ b1b1l ++ b2b2l, cf)
  where
    alp = filter (/= emptySymb) alphabet
    b1l = State "b1l"
    b1 = State "b1"
    b2l = State "b2l"
    b2 = State "b2"
    cf = State "cf"
    afb1l =
      Tr4
        { from = af,
          inAct = (Readt emptySymb, Bar, Readt emptySymb),
          to = b1l,
          outAct = (Writet emptySymb, Shift S, Writet emptySymb)
        }
    b1lb1 =
      Tr4
        { from = b1l,
          inAct = (Bar, Bar, Bar),
          to = b1,
          outAct = (Shift R, Shift S, Shift R)
        }
    b1b1l =
      flip
        map
        alp
        ( \x ->
            Tr4
              { from = b1,
                inAct = (Readt x, Bar, Readt emptySymb),
                to = b1l,
                outAct = (Writet x, Shift S, Writet x)
              }
        )
    b1b2l =
      Tr4
        { from = b1,
          inAct = (Readt emptySymb, Bar, Readt emptySymb),
          to = b2l,
          outAct = (Writet emptySymb, Shift S, Writet emptySymb)
        }
    b2lb2 =
      Tr4
        { from = b2l,
          inAct = (Bar, Bar, Bar),
          to = b2,
          outAct = (Shift R, Shift S, Shift R)
        }
    b2b2l =
      flip
        map
        alp
        ( \x ->
            Tr4
              { from = b2,
                inAct = (Readt x, Bar, Readt x),
                to = b2l,
                outAct = (Writet x, Shift S, Writet x)
              }
        )
    b2cf =
      Tr4
        { from = b2,
          inAct = (Readt emptySymb, Bar, Readt emptySymb),
          to = cf,
          outAct = (Writet emptySymb, Shift S, Writet emptySymb)
        }
