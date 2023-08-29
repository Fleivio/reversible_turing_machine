module Turing.Quadruple(Quadruple(..), getQuadruple) where

import Turing.State
import Turing.Action

data Quadruple = Quadruple {
        from   :: State,
        to     :: State,
        inAct  :: TripleInAction,
        outAct :: TripleOutAction
    }

getQuadruple :: State -> TripleInAction -> [Quadruple] -> Quadruple
getQuadruple state tia = head . filter (\q -> from q == state && inAct q == tia)
