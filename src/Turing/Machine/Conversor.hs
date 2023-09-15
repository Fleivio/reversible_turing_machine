module Turing.Machine.Conversor (toReversible, toStandard) where

import Turing.Basic.Symbol
import qualified Turing.Machine.ClassicMachine as CM
import Turing.Machine.ClassicMachine (ClassicMachine(ClassTm))
import Turing.Machine.RevMachine
import Turing.Tape.Tape
import Turing.Transition.Conversor

toReversible' :: CM.ClassicMachine -> RevMachine
toReversible' 
  ClassTm {
    CM.tape = cTape,
    CM.transitions = cTransitions,
    CM.currentState = cCurrentState,
    CM.acceptState = cAcceptState,
    CM.alphabet = cAlp
  } =
  mkTm tripleTape newTransitions cCurrentState cAcceptState cAlp
  where
    tripleTape = (cTape, mkTape emptySymb, mkTape emptySymb)
    newTransitions = computeTransitions ++ outputTransitions ++ retraceTransitions
      where 
        computeTransitions = genComputeTransitions cTransitions
        (outputTransitions, cf) = genOutputCopyTransitions cAcceptState cAlp
        -- usar o cf pra gerar o retrace
        retraceTransitions = undefined 

toReversible :: CM.ClassicMachine -> RevMachine
toReversible cm = toReversible' (toStandard cm)

toStandard :: CM.ClassicMachine -> CM.ClassicMachine
toStandard cm =
  cm {
    CM.transitions = newTransitions,
    CM.acceptState = finalState
  }
  where 
    newTransitions = CM.transitions cm ++ slTransitions
    (slTransitions, finalState) = shiftLeftTransitions (CM.acceptState cm) (CM.alphabet cm) 