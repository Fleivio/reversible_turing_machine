module Turing.Machine.RevMachine(RevMachine(..), tmRun, mkTm) where

import Turing.Tape.TripleTape
import Turing.Transition.Transition4
import Turing.Basic.State

import Turing.Machine.Machine

data RevMachine = RevTm {
        tapes :: TripleTape,
        transitions :: [Transition4],
        currentState :: State,
        acceptStates :: [State],
        halt :: Bool
    }

instance Show RevMachine where
    show tm = show t1 ++ "\n" ++
              show t2 ++ "\n" ++
              show t3 ++ "\n" ++
              show (currentState tm)
        where 
                (t1, t2, t3) = tapes tm

mkTm :: TripleTape -> [Transition4] -> State -> [State] -> RevMachine
mkTm tps trs st acc = RevTm tps trs st acc False

instance TuringMachine RevMachine where 
    tmHalt = halt

    tmStep tm | halt tm = tm
    tmStep tm@(RevTm tps trs st _ _) = 
        case transition of
            Nothing -> tm{halt = True}
            Just tr -> tmStep' tm tr
        where
            transition = getTransition st readSymbs trs
            readSymbs = tape3Read tps


tmStep' :: RevMachine -> Transition4 -> RevMachine
tmStep' tm (Transition4 _ _ nextState action) = tm{ tapes = newTapes,
                                                    currentState = nextState}
    where newTapes = tape3PerformOutAction (tapes tm) action
