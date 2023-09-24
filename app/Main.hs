module Main (main) where

import Turing.Machine.Conversor
import Turing.Machine.Machine (TuringMachine (showDefinition, tmShowRun, showStats))
import Turing.Reader (readTm)

main :: IO ()
main = do
  classicTm <- readTm "definitions/bb4.txt"
  let (logC, cMachine) = tmShowRun classicTm
  let (log1, revMachine) = tmShowRun $ toReversible classicTm

  writeFile "saida.txt" $
    "Definição da máquina clássica:\n"
      ++ showDefinition cMachine ++ "\n"
      ++ logC ++ "\n"
      ++ showStats cMachine ++ "\n\n" ++
    "Definição da máquina reversível:\n"
      ++ showDefinition revMachine ++ "\n"
      ++ log1 ++ "\n"
      ++ showStats revMachine ++ "\n\n" 