module Turing.Transition.Transition4(Transition4(..), getQuadruple) where

import Turing.Basic.State
import Turing.Tape.TripleTape
import Turing.Basic.Symbol

type SymbInAct = TripleInAction Symbol
type SymbOutAct = TripleOutAction Symbol

data Transition4 = Transition4 {
        from   :: State,
        inAct  :: SymbInAct,
        to     :: State,
        outAct :: SymbOutAct
    } deriving (Eq, Show)

hasValidCondition :: Transition4 -> State -> SymbInAct -> Bool
hasValidCondition q s tia = from q == s && inAct q == tia

getQuadruple :: State -> SymbInAct -> [Transition4] -> Transition4
getQuadruple state tia = head . filter (\z -> hasValidCondition z state tia)
