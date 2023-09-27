{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}
module Turing.Transition.Conversor
  ( genComputeTransitions,
    genOutputCopyTransitions,
    shiftLeftTransitions,
    reverseAllQuadruples,
  )
where

import Turing.Basic.Action
import Turing.Basic.State
import Turing.Basic.Symbol
import Turing.Transition.Transition4 (Transition4 (Tr4), reverseQuadruple)
import Turing.Transition.Transition5 (Transition5 (Tr5))

toQuadruple :: Transition5 -> (Transition4, Transition4)
toQuadruple
  (Tr5 stFrom stRead stTo stWrite stDir) =
    (first, second)
    where
      interState = stCombine stFrom stTo stRead
      first =
        Tr4 stFrom (Rd stRead, Bar, readEmpty) interState (Wrt stWrite, Sft R, writeEmpty)
      second =
        Tr4 interState (Bar, readEmpty, Bar) stTo (Sft stDir, Wrt interState, noShift)

genComputeTransitions :: [Transition5] -> [Transition4]
genComputeTransitions trs5 = concatMap (\(x, y) -> [x, y]) tuples
  where
    tuples = map toQuadruple trs5

reverseAllQuadruples :: State -> [State] -> [Transition4] -> [Transition4]
reverseAllQuadruples cf nStates trs = initials ++ revTrans
  where
    initials = map 
      (\n -> 
        Tr4
          cf
          (Bar, Rd n, Bar)
          (inverseState n)
          (Sft Z, Wrt emptySymb, Sft Z)
      ) nStates
    revTrans = map reverseQuadruple trs

genOutputCopyTransitions :: State -> [Symbol] -> ([Transition4], State)
genOutputCopyTransitions af alphabet =
  ([afb1l, b1lb1, b1b2l, b2lb2, b2cf] ++ b1b1l ++ b2b2l, cf)
  where
    mapAlphabet = flip map $ filter (/= emptySymb) alphabet
    b1l = "b1l"
    b1 = "b1"
    b2l = "b2l"
    b2 = "b2"
    cf = "cf"

    afb1l = Tr4 af empBarEmp b1l empSftEmp
    b1lb1 = Tr4 b1l bar3 b1 (Sft R, noShift, Sft R)
    b1b2l = Tr4 b1 empBarEmp b2l empSftEmp
    b2lb2 = Tr4 b2l bar3 b2 (Sft L, noShift, Sft L)
    b1b1l =
      mapAlphabet
        ( \x ->
            Tr4 b1 (Rd x, Bar, readEmpty) b1l (Wrt x, noShift, Wrt x)
        )
    b2b2l =
      mapAlphabet
        ( \x ->
            Tr4 b2 (Rd x, Bar, Rd x) b2l (Wrt x, noShift, Wrt x)
        )
    b2cf = Tr4 b2 empBarEmp cf empSftEmp

    empBarEmp = (readEmpty, Bar, readEmpty)
    bar3 = (Bar, Bar, Bar)
    empSftEmp = (writeEmpty, Sft Z, writeEmpty)

shiftLeftTransitions :: State -> [Symbol] -> ([Transition5], State)
shiftLeftTransitions intermediate alph = (transitions, finalState)
  where
    transitions =  sl1b : sl2b : sr2b : sl3b : tr1 ++ sl1a ++ sr2a ++ sl2a ++ sl3a
    finalState = "af"
    sl1 = "sl1"
    sl2 = "sl2"
    sl3 = "sl3"
    sr2 = "sr2"
    mapBAlphabet = flip map alph
    mapAlphabet = flip map $ filter (/= emptySymb) alph
    tr1 =
      mapBAlphabet
        $ \x -> Tr5 intermediate x sl1 x L
    sl1a =
      mapAlphabet
        $ \x -> Tr5 sl1 x sl2 x L
    sl1b =
      Tr5 sl1 emptySymb sl3 emptySymb L
    sl3b =
      Tr5 sl3 emptySymb sr2 emptySymb R
    sl3a =
      mapAlphabet
        $ \x -> Tr5 sl3 x sl2 x L
    sl2a =
      mapAlphabet
        $ \x -> Tr5 sl2 x sl2 x L
    sl2b =
      Tr5 sl2 emptySymb finalState emptySymb Z
    sr2b = 
      Tr5 sr2 emptySymb sr2 emptySymb R
    sr2a = 
      mapAlphabet
        $ \x -> Tr5 sr2 x finalState x L