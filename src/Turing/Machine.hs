module Turing.Machine(TuringMachine(..)) where

import Turing.Tape.TripleTape
import Turing.Transition.Transition4
import Turing.Basic.State

data TuringMachine = TM {
        tapes :: TripleTape,
        transitions :: [Transition4],
        currentState :: State,
        acceptStates :: [State],
        halt :: Bool
    }

tmStep :: TuringMachine -> TuringMachine
tmStep tm@(TM _ _ _ _ True) = tm
tmStep tm@(TM tps trs st _ _) = undefined
    where
        transition = getTransition st readSymbols (transitions tm)
        readSymbols = tape3Read tps