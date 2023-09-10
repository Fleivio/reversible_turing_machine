module Turing.Machine.ClassicMachine(ClassicMachine(..), tmRun, mkTmClassic) where

import Turing.Basic.State
import Turing.Transition.Transition5

import Turing.Tape.Tape

import Turing.Machine.Machine

data ClassicMachine = ClassTm {
        tape :: Tape,
        transitions :: [Transition5],
        currentState :: State,
        acceptState :: State,
        counter :: Int,
        halt :: Bool
    }

instance Show ClassicMachine where
    show tm = unlines (map show (transitions tm)) ++
              show (tape tm) ++ "\n" ++
              show (currentState tm) ++ "\n" ++
              show (counter tm)

instance TuringMachine ClassicMachine where
    tmHalt = halt

    tmStep tm | halt tm = tm
    tmStep tm@(ClassTm tp trs st _ _ _) = 
        case transition of
            Nothing -> tm{halt = True}
            Just tr -> tmStep' tm tr
        where
            transition = getTransition st readSymbs trs
            readSymbs = tapeRead tp

mkTmClassic :: Tape -> [Transition5] -> State -> State -> ClassicMachine
mkTmClassic tp trs st acc = ClassTm tp trs st acc 0 False

tmStep' :: ClassicMachine -> Transition5 -> ClassicMachine
tmStep' tm (Transition5 _ _ nextState writeSymbs dir1) = 
    tm{ tape = newTape,
        currentState = nextState,
        counter = counter tm + 1}
    where newTape = tapeShift (tapeWrite (tape tm) writeSymbs) dir1