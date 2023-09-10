module Turing.Machine.Conversor(toReversible) where

import qualified Turing.Machine.RevMachine as RM
import qualified Turing.Machine.ClassicMachine as CM

import qualified Turing.Transition.Transition4 as T4
import qualified Turing.Transition.Transition5 as T5

import Turing.Transition.Conversor
import Turing.Tape.Tape
import Turing.Basic.Symbol

import Turing.Basic.State


toReversible :: CM.ClassicMachine -> RM.RevMachine
toReversible cm = RM.mkTm tripleTape newTransitions (CM.currentState cm) (CM.acceptState cm)
    where
        newTransitions = computeTransitions ++ outputTransitions
        computeTransitions = genComputeTransitions (CM.transitions cm)
        (outputTransitions, cf) = genOutputCopyTransitions (CM.acceptState cm) ["a", "b"]
        
        tripleTape = (CM.tape cm, mkTape memptySymbol, mkTape memptySymbol )




-- inBetweenState :: State
-- inBetweenState = State "inBetweenState"

-- reverseTransitions :: [T4.Transition4]
-- reverseTransitions = undefined 