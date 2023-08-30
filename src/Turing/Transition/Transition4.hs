module Turing.Transition.Transition4(Transition4(..), getQuadruple) where

import Turing.Basic.State
import Turing.Tape.TripleTape

data Transition4 = Transition4 {
        from   :: State,
        inAct  :: TripleInAction,
        to     :: State,
        outAct :: TripleOutAction
    } deriving (Eq, Show)

hasValidCondition :: Transition4 -> State -> TripleInAction -> Bool
hasValidCondition q s tia = from q == s && inAct q == tia

getQuadruple :: State -> TripleInAction -> [Transition4] -> Transition4
getQuadruple state tia = head . filter (\z -> hasValidCondition z state tia)
