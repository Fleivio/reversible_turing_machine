module Turing.Machine.ClassicMachine() where

import Turing.Basic.State
import Turing.Basic.Symbol
import Turing.Transition.Transition5

import Turing.Machine.Machine

data ClassicMachine = ClassTm {
        tape :: Tape,
        transitions :: [Transition5],
        currentState :: State,
        acceptStates :: [State],
        halt :: Bool
    }

instance TuringMachine ClassicMachine where
    tmHalt = halt

    tmStep tm | halt tm = tm
    tmStep tm@(ClassTm tp trs st _ _) = 
        case transition of
            Nothing -> tm{halt = True}
            Just tr -> tmStep' tm tr
        where
            transition = getTransition st readSymbs trs
            readSymbs = tapeRead tp

tmStep' :: ClassicMachine -> Transition5 -> ClassicMachine
tmStep' tm (Transition5 _ _ nextState writeSymbs dir) = 
    tm{ tape = newTape,
        currentState = nextState}
    where newTape = tapeShift (tapeWrite (tape tm) writeSymbs) dir