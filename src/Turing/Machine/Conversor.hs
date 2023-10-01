module Turing.Machine.Conversor (toReversible, toStandard) where

import Turing.Basic.State (inverseState)
import Turing.Basic.Symbol
import Turing.Machine.ClassicMachine
import Turing.Machine.Machine
import Turing.Machine.RevMachine (RevMachine)
import Turing.Tape.Tape
import Turing.Transition.Conversor
import Turing.Transition.Transition
import Turing.Transition.Transition4


toReversible' :: ClassicMachine -> RevMachine
toReversible' c@ClassTm { tape = cTape,
                          currentState = cCurrentState,
                          alphabet = cAlp }
              = mkTm tripleTape newTransitions cCurrentState lastState cAlp
  where
    tripleTape = (cTape, mkTape emptySymb, mkTape emptySymb)
    lastState = inverseState cCurrentState
    newTransitions = genNewTransitions c

genNewTransitions :: ClassicMachine -> [Transition4]
genNewTransitions ClassTm { transitions = cTransitions,
                            acceptState = cAccSt,
                            alphabet = cAlp }
                  = computeTransitions ++ outputTransitions ++ retraceTransitions
  where
    nStates = map from $ getTransitionThatGoesTo cAccSt computeTransitions
    computeTransitions = genComputeTransitions cTransitions
    (outputTransitions, cf) = genOutputCopyTransitions cAccSt cAlp
    retraceTransitions = reverseAllQuadruples cf nStates computeTransitions

toReversible :: ClassicMachine -> RevMachine
toReversible cm = toReversible' (toStandard cm)

toStandard :: ClassicMachine -> ClassicMachine
toStandard cm = cm { transitions = transitions cm ++ slTransitions,
                     acceptState = finalState }
  where
    (slTransitions, finalState) = shiftLeftTransitions (acceptState cm) (alphabet cm)