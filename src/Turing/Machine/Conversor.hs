module Turing.Machine.Conversor(toReversible) where

import qualified Turing.Machine.RevMachine as RM
import qualified Turing.Machine.ClassicMachine as CM

import qualified Turing.Transition.Transition4 as T4
import qualified Turing.Transition.Transition5 as T5

import Turing.Transition.Conversor
import Turing.Tape.Tape
import Turing.Basic.Symbol

import Turing.Basic.State

-- mkTm :: TripleTape -> [Transition4] -> State -> [State] -> RevMachine

toReversible :: CM.ClassicMachine -> RM.RevMachine
toReversible cm = RM.mkTm tripleTape newTransitions (CM.currentState cm) (CM.acceptState cm)
    where
        newTransitions = concatMap (\(x, y) -> [x, y]) tuples
            where tuples = map toQuadruple (CM.transitions cm)
        tripleTape = (CM.tape cm, mkTape (mempty :: Symbol), mkTape (mempty :: Symbol) )

-- outputCopyTransitions :: [T4.Transition4]
-- outputCopyTransitions = undefined

-- inBetweenState :: State
-- inBetweenState = State "inBetweenState"

-- reverseTransitions :: [T4.Transition4]
-- reverseTransitions = undefined 