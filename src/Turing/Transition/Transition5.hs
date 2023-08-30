module Turing.Transition.Transition5(Transition5(..), intermediateState) where

import Turing.Basic.State
import Turing.Basic.Symbol
import Turing.Tape.Basic.Direction

data Transition5 = Transition5 {
        from :: State,
        to   :: State,
        rSym  :: Symbol,
        wSym  :: Symbol,
        dir   :: Direction
    } deriving (Eq, Show)

intermediateState :: Transition5 -> State
intermediateState q = from q `stCombine` to q