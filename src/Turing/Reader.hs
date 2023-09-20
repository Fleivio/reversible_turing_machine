module Turing.Reader (readTm) where

import Turing.Basic.State (State (..))
import Turing.Basic.Direction (Direction (L, R))
import Turing.Transition.Transition5 (Transition5 (..))
import Turing.Machine.ClassicMachine (ClassicMachine)
import Turing.Tape.Tape (mkTapeFromList)
import Turing.Basic.Symbol
import Turing.Machine.Machine
import Utils

mkTransition :: String -> Transition5
mkTransition str =
  Tr5
    { from5= State $ head symbols,
      rSym = symbols !! 1,
      to5  = State $ symbols !! 2,
      wSym = symbols !! 3,
      dir  = if symbols !! 4 == "R" then R else L
    }
  where
    symbols = splitBy ',' $ replace '=' ',' $ filter (`notElem` "()") str

mkMachineDefinition :: [String] -> ([State], [String], [String], [Transition5], String)
mkMachineDefinition machineString = (states, alphabet, tapeAlphabet, transitions, input)
  where
    states       = map State (words $ machineString !! 1)
    alphabet     = words $ machineString !! 2
    tapeAlphabet = words $ machineString !! 3
    transitions  = map mkTransition (init $ drop 4 machineString)
    input        = last machineString

readTm :: String -> IO ClassicMachine
readTm filePath = do
  file <- readFile filePath
  let (states, _, tapeAlphabet, transitions', input) = mkMachineDefinition $ lines file
  let tape' = mkTapeFromList emptySymb (map (: []) input)
  let tm    = mkTm tape' transitions' (head states) (last states) tapeAlphabet
  return tm