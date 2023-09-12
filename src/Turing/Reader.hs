module Turing.Reader (mkMachineDefinition) where

import Turing.Basic.State (State (..))
import Turing.Tape.Basic.Direction (Direction (L, R))
import Turing.Transition.Transition5 (Transition5 (..))

replace :: (Eq a) => a -> a -> [a] -> [a]
replace something replacement =
  map (\x -> if x == something then replacement else x)

splitBy :: Char -> String -> [String]
splitBy by =
  foldr (\x acc -> if x == by then "" : acc else (x : head acc) : tail acc) [""]

mkTransition :: String -> Transition5
mkTransition str =
  Transition5
    { from = State $ head symbols,
      rSym = symbols !! 1,
      to = State $ symbols !! 2,
      wSym = symbols !! 3,
      dir = if symbols !! 4 == "R" then R else L
    }
  where
    symbols = splitBy ',' $ replace '=' ',' $ filter (`notElem` "()") str

mkMachineDefinition :: [String] -> ([State], [String], [String], [Transition5], String)
mkMachineDefinition machineString = (states, alphabet, tapeAlphabet, transitions, input)
  where
    states = map State (words $ machineString !! 1)
    alphabet = words $ machineString !! 2
    tapeAlphabet = words $ machineString !! 3
    transitions = map mkTransition (init $ drop 4 machineString)
    input = last machineString