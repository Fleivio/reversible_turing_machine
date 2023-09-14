module Turing.Machine.Conversor (toReversible, toStandard) where

import Turing.Basic.Symbol
import qualified Turing.Machine.ClassicMachine as CM
import Turing.Machine.RevMachine
import Turing.Tape.Tape
import Turing.Transition.Conversor
import Turing.Basic.State

toReversible :: CM.ClassicMachine -> RevMachine
toReversible cm' =
  mkTm tripleTape newTransitions (CM.currentState cm) (CM.acceptState cm) alp
  where
    cm = toStandard cm'
    alp = CM.alphabet cm
    newTransitions = computeTransitions ++ outputTransitions
    computeTransitions = genComputeTransitions (CM.transitions cm)
    (outputTransitions, _) = genOutputCopyTransitions (CM.acceptState cm) alp

    tripleTape = (CM.tape cm, mkTape emptySymb, mkTape emptySymb)

toStandard :: CM.ClassicMachine -> CM.ClassicMachine
toStandard cm =
  cm {
    CM.transitions = newTransitions,
    CM.acceptState = finalState
  }
  where 
    newTransitions = CM.transitions cm ++ slTransitions
    (slTransitions, finalState) = shiftLeftTransitions (CM.acceptState cm) (CM.alphabet cm) 