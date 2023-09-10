module Turing.Reader (mkMachineDefinition) where

import Turing.Transition.Transition5 ( Transition5(..) )
import Turing.Basic.State ( State(..) )
import Turing.Tape.Basic.Direction ( Direction(L, R) )

replace :: (Eq a) => a -> a -> [a] -> [a]
replace something replacement = 
    map (\x -> if x == something then replacement else x)

splitBy :: Char -> String -> [String]
splitBy by = 
    foldl (\acc x -> if x == by then "" : acc else (x : head acc) : tail acc) [""]
 
mkTransition :: String -> Transition5
mkTransition str =
    Transition5 { from = State $ symbols !! 1
                , rSym = symbols !! 2
                , to   = State $ symbols !! 3
                , wSym = symbols !! 4
                , dir  = if symbols !! 5 == "R" then R else L
                }
    where
        symbols = splitBy ',' $ replace '=' ',' $ filter (`notElem` "()") str


mkMachineDefinition :: [String] -> ([State], [String], [String], [Transition5], String)
mkMachineDefinition machineString = (states, alphabet, tapeAlphabet, transitions, input)
    where
        states = map State (words $ machineString !! 2)
        alphabet = words $ machineString !! 3
        tapeAlphabet = words $ machineString !! 4
        transitions = map mkTransition (init $ drop 4 machineString) 
        input = last machineString