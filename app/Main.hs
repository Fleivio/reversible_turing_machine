module Main (main) where

import Turing.Basic.Symbol
import Turing.Machine.ClassicMachine
import Turing.Machine.Conversor
import Turing.Reader (mkMachineDefinition)
import Turing.Tape.Tape

readTm :: String -> IO ClassicMachine
readTm fileStr = do
  file <- readFile fileStr
  let (states, _, tapeAlphabet, transitions', input) = mkMachineDefinition $ lines file
  let tape' = mkTapeFromList memptySymbol (map (: []) input)
  let tm = mkTmClassic tape' transitions' (head states) (last states) tapeAlphabet
  return tm

main :: IO ()
main = do
  classicTm <- readTm "entrada-quintupla.txt"
  print $ tmRun $ toReversible classicTm