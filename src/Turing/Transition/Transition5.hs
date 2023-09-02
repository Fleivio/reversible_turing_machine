module Turing.Transition.Transition5(Transition5(..), intermediateState, hasValidCondition, getTransition) where

import Turing.Basic.State
import Turing.Basic.Symbol
import Turing.Tape.Basic.Direction

data Transition5 = Transition5 {
        from  :: State,
        rSym  :: Symbol,
        to    :: State,
        wSym  :: Symbol,
        dir   :: Direction
    } deriving (Eq, Show)

intermediateState :: Transition5 -> State
intermediateState q = from q `stCombine` to q

hasValidCondition :: Transition5 -> State -> Symbol -> Bool
hasValidCondition q s sym = from q == s && rSym q == sym

getTransition :: State -> Symbol -> [Transition5] -> Maybe Transition5
getTransition state sym trs =   if null validCases
                                then Nothing 
                                else Just $ head validCases
    where 
         validCases = filter (\z -> hasValidCondition z state sym) trs