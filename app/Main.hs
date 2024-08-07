module Main (main) where

import Turing.Machine.Conversor
import Turing.Machine.ClassicMachine (ClassicMachine)
import Turing.Machine.Machine
import Turing.Reader (readTm)
import System.Environment

runAll :: ClassicMachine -> IO ()
runAll classicTm = do
  let (cMachine, logC) = runWriter $ tmShowRun classicTm
      (revMachine, log1) = runWriter . tmShowRun $ toReversible classicTm

  writeFile "saida.txt" $
    "Definição da máquina clássica:\n"
      ++ showDefinition cMachine ++ "\n"
      ++ logC ++ "\n"
      ++ showStats cMachine ++ "\n\n" ++
    "Definição da máquina reversível:\n"
      ++ showDefinition revMachine ++ "\n"
      ++ log1 ++ "\n"
      ++ showStats revMachine ++ "\n\n"
      

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      classicTm <- readTm fileName
      runAll classicTm
    _ -> do
      classicTm <- readTm "entrada-quintupla.txt"
      runAll classicTm
