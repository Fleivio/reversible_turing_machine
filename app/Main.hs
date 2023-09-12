module Main (main) where

import Turing.Machine.Conversor
import Turing.Reader (readTm)
import Turing.Machine.Machine


main :: IO ()
main = do
  classicTm <- readTm "entrada-quintupla.txt"
  print $ tmRun $ toReversible classicTm