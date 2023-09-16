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
import qualified Turing.Transition.Transition5 as T5

toQuadruple :: T5.Transition5 -> (Transition4, Transition4)
toQuadruple
  Tr5
    { T5.from = stFrom,
      T5.to   = stTo,
      T5.dir  = stDir,
      T5.rSym = stRead,
      T5.wSym = stWrite
    } =
    (first, second)
    where
      interState = stCombine stFrom stTo stRead
      first =
        Tr4
          { from   = stFrom,
            inAct  = (Readt stRead, Bar, readEmpty),
            to     = interState,
            outAct = (Writet stWrite, Shift R, writeEmpty)
          }
      second =
        Tr4
          { from  = interState,
            inAct  = (Bar, readEmpty, Bar),
            to     = stTo,
            outAct = (Shift stDir, Writet (stGetName interState), noShift)
          }

genComputeTransitions :: [T5.Transition5] -> [Transition4]
genComputeTransitions trs5 = concatMap (\(x, y) -> [x, y]) tuples
  where
    tuples = map toQuadruple trs5

genReverseTransitions :: State -> State -> [Transition4] -> [Transition4]
genReverseTransitions cf nState trs = initial : revTrans
  where
    initial =
      Tr4
        { from   = cf,
          inAct  = (Bar, Readt (stGetName nState), Bar),
          to     = inverseState nState,
          outAct = (Shift S, Writet emptySymb, Shift S)
        }
    revTrans = map reverseQuadruple trs

genOutputCopyTransitions :: State -> [Symbol] -> ([Transition4], State)
genOutputCopyTransitions af alphabet =
  ([afb1l, b1lb1, b1b2l, b2lb2, b2cf] ++ b1b1l ++ b2b2l, cf)
  where
    mapAlphabet = flip map $ filter (/= emptySymb) alphabet
    b1l = State "b1l"
    b1  = State "b1"
    b2l = State "b2l"
    b2  = State "b2"
    cf  = State "cf"

    afb1l = Tr4 af empBarEmp b1l empSftEmp
    b1lb1 = Tr4 b1l bar3 b1 (Shift R, noShift, Shift R)
    b1b2l = Tr4 b1 empBarEmp b2l empSftEmp
    b2lb2 = Tr4 b2l bar3 b2 (Shift L, noShift, Shift L)
    b1b1l =
      mapAlphabet
        ( \x ->
            Tr4 b1 (Readt x, Bar, readEmpty) b1l (Writet x, noShift, Writet x)
        )
    b2b2l =
      mapAlphabet
        ( \x ->
            Tr4 b2 (Readt x, Bar, Readt x) b2l (Writet x, noShift, Writet x)
        )
    b2cf = Tr4 b2 empBarEmp cf empSftEmp

    empBarEmp = (readEmpty, Bar, readEmpty)
    bar3      = (Bar, Bar, Bar)
    empSftEmp = (writeEmpty, Shift S, writeEmpty)

shiftLeftTransitions :: State -> [Symbol] -> ([Transition5], State)
shiftLeftTransitions intermediate alph = (transitions, finalState)
  where
    transitions  = tr1 ++ tr2 ++ [tr3]
    finalState   = State "af"
    st1          = State "st1"
    mapBAlphabet = flip map alph
    mapAlphabet  = flip map $ filter (/= emptySymb) alph
    tr1 =
      mapBAlphabet
        (\x -> Tr5 intermediate x st1 x L)
    tr2 =
      mapAlphabet
        (\x -> Tr5 st1 x st1 x L)
    tr3 =
      Tr5 st1 emptySymb finalState emptySymb S
