module Turing.Machine.Conversor (toReversible) where

import Turing.Basic.Symbol
import qualified Turing.Machine.ClassicMachine as CM
import qualified Turing.Machine.RevMachine as RM
import Turing.Tape.Tape
import Turing.Transition.Conversor

toReversible :: CM.ClassicMachine -> RM.RevMachine
toReversible cm =
  RM.mkTm tripleTape newTransitions (CM.currentState cm) (CM.acceptState cm) alp
  where
    alp = CM.alphabet cm
    newTransitions = computeTransitions ++ outputTransitions
    computeTransitions = genComputeTransitions (CM.transitions cm)
    (outputTransitions, _) = genOutputCopyTransitions (CM.acceptState cm) alp

    tripleTape = (CM.tape cm, mkTape emptySymb, mkTape emptySymb)