module Main (main) where

import Turing.Machine.Conversor
import Turing.Machine.Machine (TuringMachine (showDefinition, tmShowRun))
import Turing.Reader (readTm)

main :: IO ()
main = do
  classicTm <- readTm "entrada-quintupla.txt"
  let (log, revMachine) = tmShowRun $ toReversible classicTm

  writeFile "saida.txt" $
    "Definição da máquina reversível:\n\n"
      ++ showDefinition revMachine
      ++ "\n\n"
      ++ "Log de avaliação:\n\n"
      ++ log