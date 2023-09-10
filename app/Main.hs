module Main (main) where

import Turing.Machine.ClassicMachine
import Turing.Tape.Tape
import Turing.Basic.State
import Turing.Transition.Transition5
--import Turing.Machine.Conversor

import Turing.Tape.Basic.Direction

--import qualified Turing.Machine.RevMachine as RM

import Turing.Reader (mkMachineDefinition)

tp1 :: Tape
tp1 = mkTape "0"

q0 :: State
q0 = State "0"
q1 :: State
q1 = State "1"
q2 :: State
q2 = State "2"
q3 :: State
q3 = State "3"
qH :: State
qH = State "H"

tab' :: [Transition5]
tab' = [
        Transition5 q0 "0" q1 "1" R,
        Transition5 q0 "1" q1 "1" L,
        Transition5 q1 "0" q0 "1" L,
        Transition5 q1 "1" q2 "0" L,
        Transition5 q2 "0" qH "1" R,
        Transition5 q2 "1" q3 "1" L,
        Transition5 q3 "0" q3 "1" R,
        Transition5 q3 "1" q0 "0" R
        ]

-- tTest :: RM.RevMachine
tTest = toReversible $ mkTmClassic tp1 tab' q0 qH

main :: IO ()
main = do
        file <- readFile "../entradas/entrada-quintupla.txt"
        mkMachineDefinition $ lines file

--main = print $ tmRun tTest
       
