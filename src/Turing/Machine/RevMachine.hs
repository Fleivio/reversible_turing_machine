module Turing.Machine.RevMachine (RevMachine (..), tmRun, mkTm) where

import Turing.Basic.State
import Turing.Basic.Symbol
import Turing.Machine.Machine
import Turing.Tape.TripleTape
import Turing.Transition.Transition4

data RevMachine = RevTm
  { tapes :: TripleTape,
    transitions :: [Transition4],
    currentState :: State,
    acceptState :: State,
    counter :: Int,
    alphabet :: [Symbol],
    halt :: Bool
  }

instance Show RevMachine where
  show tm =
    unlines (map show (transitions tm))
      ++ show t1 ++ "\n"
      ++ show t2 ++ "\n"
      ++ show t3 ++ "\n"
      ++ show (currentState tm) ++ "\n"
      ++ show (counter tm)
    where
      (t1, t2, t3) = tapes tm

mkTm :: TripleTape -> [Transition4] -> State -> State -> [Symbol] -> RevMachine
mkTm tps trs st acc alp = RevTm tps trs st acc 0 alp False

instance TuringMachine RevMachine where
  tmHalt = halt

  tmStep tm | halt tm = tm
  tmStep tm@(RevTm tps trs st _ _ _ _) =
    case transition of
      Nothing -> tm {halt = True}
      Just tr -> tmStep' tm tr
    where
      transition = getTransition st readSymbs trs
      readSymbs = tape3Read tps

tmStep' :: RevMachine -> Transition4 -> RevMachine
tmStep' tm (Tr4 _ _ nextState action) =
  tm
    { tapes = newTapes,
      currentState = nextState,
      counter = counter tm + 1
    }
  where
    newTapes = tape3Perform (tapes tm) action
