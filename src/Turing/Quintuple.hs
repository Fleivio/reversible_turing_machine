module Turing.Quintuple(Quintuple(..), intermediateState) where

import Turing.State
import Turing.Symbol
import Turing.Action

data Quintuple = Quintuple {
        from :: State,
        to   :: State,
        rSym  :: Symbol,
        wSym  :: Symbol,
        dir   :: Direction
    }

intermediateState :: Quintuple -> State
intermediateState q = from q `stCombine` to q