module Main (main) where

import Turing.Machine
import Turing.Tape.TripleTape
import Turing.Transition.Transition4
import Turing.Basic.State
import Turing.Tape.Basic.Direction
import Turing.Tape.Basic.Action

tTapes :: TripleTape
tTapes = mkTape3FromLists ("", "", "") (["a", "a", "b"], [], [])

q0 = State "q0"
q1 = State "q1"
q2 = State "q2"

tTransitions :: [Transition4]
tTransitions = [
        Transition4 q0 (Readt "a", Bar, Bar) q1 (Writet "a", Shift S, Shift S),
        Transition4 q0 (Readt "b", Bar, Bar) q2 (Shift R, Shift S, Shift S),
        Transition4 q1 (Bar, Bar, Bar) q0 (Shift R, Shift S, Shift S)
    ]

tTest :: TuringMachine
tTest = mkTm tTapes tTransitions q0 [q2]

main :: IO ()
main = print $ tmRun tTest 
