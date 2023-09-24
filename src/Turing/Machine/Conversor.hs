module Turing.Machine.Conversor (toReversible, toStandard) where

import Turing.Basic.Symbol
import Turing.Basic.State (inverseState)
import Turing.Machine.ClassicMachine 
import Turing.Machine.RevMachine (RevMachine)
import Turing.Tape.Tape
import Turing.Transition.Conversor
import Turing.Machine.Machine
import Turing.Transition.Transition

toReversible' :: ClassicMachine -> RevMachine
toReversible' 
  ClassTm {
    tape         = cTape,
    transitions  = cTransitions,
    currentState = cCurrentState,
    acceptState  = cAcceptState,
    alphabet     = cAlp
  } =
  mkTm tripleTape newTransitions cCurrentState lastState cAlp
  where
    tripleTape     = (cTape, mkTape emptySymb, mkTape emptySymb)
    lastState      = inverseState cCurrentState
    newTransitions = computeTransitions ++ outputTransitions ++ retraceTransitions
      where
        nState                  = from $ getTransitionThatGoesTo cAcceptState computeTransitions
        computeTransitions      = genComputeTransitions cTransitions
        (outputTransitions, cf) = genOutputCopyTransitions cAcceptState cAlp
        retraceTransitions      = genReverseTransitions cf nState computeTransitions

toReversible :: ClassicMachine -> RevMachine
toReversible cm = toReversible' (toStandard cm)

toStandard :: ClassicMachine -> ClassicMachine
toStandard cm =
  cm {
    transitions = transitions cm ++ slTransitions,
    acceptState = finalState
  }
  where 
    (slTransitions, finalState) = shiftLeftTransitions (acceptState cm) (alphabet cm) 