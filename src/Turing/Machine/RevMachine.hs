module Turing.Machine.RevMachine (RevMachine (..), mkTm) where

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
       show t1 ++ " "
      ++ show t2 ++ " "
      ++ show t3 ++ " "
      ++ show (currentState tm)
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

  tmAcceptSt = acceptState
  tmCurrentSt = currentState
  showDefinition = showTrs4 . transitions

tmStep' :: RevMachine -> Transition4 -> RevMachine
tmStep' tm (Tr4 _ _ nextState action) =
  tm
    { tapes        = newTapes,
      currentState = nextState,
      counter      = counter tm + 1
    }
  where
    newTapes = tape3Perform (tapes tm) action

showTrs4 :: [Transition4] -> String
showTrs4 trs = unlines $ map alignTr trs
  where
    alignTr tr = "(" ++
                  alignF from tr ++ ", " ++ alignF inAct tr  ++ ") -> (" ++
                  alignF to tr   ++ ", " ++ alignF outAct tr ++ ")" 
    nSpaces n = replicate n ' '
    alignF f tr = string ++ nSpaces (maxSize f - length string)
      where 
        string = show (f tr)
    maxSize f = maximum $ map (length . show . f) trs