module Turing.Machine(TuringMachine(..), tmRun, mkTm) where

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

instance Show TuringMachine where
    show tm = show t1 ++ "\n" ++
              show t2 ++ "\n" ++
              show t3 ++ "\n" ++
              show (currentState tm)
        where 
                (t1, t2, t3) = tapes tm

mkTm :: TripleTape -> [Transition4] -> State -> [State] -> TuringMachine
mkTm tps trs st acc = TM tps trs st acc False

tmStep' :: TuringMachine -> Transition4 -> TuringMachine
tmStep' tm (Transition4 _ _ nextState action) = tm{ tapes = newTapes,
                                                    currentState = nextState}
    where newTapes = tape3PerformOutAction (tapes tm) action

tmStep :: TuringMachine -> TuringMachine
tmStep tm | halt tm = tm
tmStep tm@(TM tps trs st _ _) = 
    case transition of
        Nothing -> tm{halt = True}
        Just tr -> tmStep' tm tr
    where
        transition = getTransition st readSymbs trs
        readSymbs = tape3Read tps

tmRun :: TuringMachine -> TuringMachine
tmRun tm@(TM _ _ _ _ True) = tm
tmRun tm = tmRun $ tmStep tm