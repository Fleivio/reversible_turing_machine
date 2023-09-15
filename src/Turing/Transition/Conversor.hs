{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}
module Turing.Transition.Conversor
  ( genComputeTransitions,
    genOutputCopyTransitions,
    shiftLeftTransitions,
    genReverseTransitions,
  )
where

import Turing.Basic.Action
import Turing.Basic.Direction
import Turing.Basic.State
import Turing.Basic.Symbol
import Turing.Transition.Transition4
import Turing.Transition.Transition5 (Transition5 (Tr5))
import Turing.Transition.Transition5 qualified as T5

toQuadruple :: T5.Transition5 -> (Transition4, Transition4)
toQuadruple
  Tr5
    { T5.from = stFrom,
      T5.to = stTo,
      T5.dir = stDir,
      T5.rSym = stRead,
      T5.wSym = stWrite
    } =
    (first, second)
    where
      interState = stCombine stFrom stTo stRead
      first =
        Tr4
          { from = stFrom,
            inAct = (Readt stRead, Bar, readEmpty),
            to = interState,
            outAct = (Writet stWrite, Shift R, writeEmpty)
          }
      second =
        Tr4
          { from = interState,
            inAct = (Bar, readEmpty, Bar),
            to = stTo,
            outAct = (Shift stDir, Writet (stGetName interState), noShift)
          }

genComputeTransitions :: [T5.Transition5] -> [Transition4]
genComputeTransitions trs5 = concatMap (\(x, y) -> [x, y]) tuples
  where
    tuples = map toQuadruple trs5

reverseQuadruple :: Transition4 -> Transition4
reverseQuadruple
  Tr4
    { from = stFrom,
      inAct = (Readt r1, Bar, _),
      to = stTo,
      outAct = (Writet w1, Shift R, _)
    } =
    Tr4
      { from = inverseState stTo,
        inAct = (Readt w1, Bar, readEmpty),
        to = inverseState stFrom,
        outAct = (Writet r1, Shift L, writeEmpty)
      }
reverseQuadruple
  Tr4
    { from = stFrom,
      inAct = (Bar, _, Bar),
      to = stTo,
      outAct = (Shift dir, Writet interS, Shift S)
    } =
    Tr4
      { from = inverseState stTo,
        inAct = (Bar, Readt interS, Bar),
        to = inverseState stFrom,
        outAct = (Shift (revDir dir), writeEmpty, Shift S)
      }
reverseQuadruple _ = error "Invalid Quadruple"

inverseState :: State -> State
inverseState s = State $ "inv." ++ stGetName s

genReverseTransitions :: State -> State -> [Transition4] -> [Transition4]
genReverseTransitions cf nState trs = revTrans ++ [initial]
  where
    initial =
      Tr4
        { from = cf,
          inAct = (Bar, Readt (stGetName nState), Bar),
          to = inverseState nState,
          outAct = (Shift S, Writet emptySymb, Shift S)
        }
    revTrans = map reverseQuadruple trs

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
          inAct = (readEmpty, Bar, readEmpty),
          to = b1l,
          outAct = (writeEmpty, noShift, writeEmpty)
        }
    b1lb1 =
      Tr4
        { from = b1l,
          inAct = (Bar, Bar, Bar),
          to = b1,
          outAct = (Shift R, noShift, Shift R)
        }
    b1b1l =
      flip
        map
        alp
        ( \x ->
            Tr4
              { from = b1,
                inAct = (Readt x, Bar, readEmpty),
                to = b1l,
                outAct = (Writet x, noShift, Writet x)
              }
        )
    b1b2l =
      Tr4
        { from = b1,
          inAct = (readEmpty, Bar, readEmpty),
          to = b2l,
          outAct = (writeEmpty, noShift, writeEmpty)
        }
    b2lb2 =
      Tr4
        { from = b2l,
          inAct = (Bar, Bar, Bar),
          to = b2,
          outAct = (Shift L, noShift, Shift L)
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
                outAct = (Writet x, noShift, Writet x)
              }
        )
    b2cf =
      Tr4
        { from = b2,
          inAct = (readEmpty, Bar, readEmpty),
          to = cf,
          outAct = (writeEmpty, noShift, writeEmpty)
        }

shiftLeftTransitions :: State -> [Symbol] -> ([Transition5], State)
shiftLeftTransitions intermediate alph = (transitions, finalState)
  where
    transitions = tr1 ++ tr2 ++ [tr3]
    finalState = State "af"
    st1 = State "st1"
    tr1 =
      map
        ( \x ->
            Tr5
              { T5.from = intermediate,
                T5.rSym = x,
                T5.to = st1,
                T5.wSym = x,
                T5.dir = L
              }
        )
        alph
    tr2 =
      map
        ( \x ->
            Tr5
              { T5.from = st1,
                T5.rSym = x,
                T5.to = st1,
                T5.wSym = x,
                T5.dir = L
              }
        )
        $ filter (/= emptySymb) alph
    tr3 =
      Tr5
        { T5.from = st1,
          T5.rSym = emptySymb,
          T5.to = finalState,
          T5.wSym = emptySymb,
          T5.dir = S
        }
