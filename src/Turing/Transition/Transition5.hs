module Turing.Transition.Transition5(Transition5(..), hasValidCondition, getTransition) where

import Turing.Basic.State
import Turing.Basic.Symbol
import Turing.Tape.Basic.Direction ( Direction )

data Transition5 = Transition5 {
        from  :: State,
        rSym  :: Symbol,
        to    :: State,
        wSym  :: Symbol,
        dir   :: Direction
    } deriving (Eq)

instance Show Transition5 where
    show (Transition5 f rs t ws d) =
        "(" ++ show f ++ ", " ++ show rs ++ ") -> (" ++ show t ++ ", " ++ show ws ++ ", " ++ show d ++ ")"

hasValidCondition :: Transition5 -> State -> Symbol -> Bool
hasValidCondition q s sym = from q == s && rSym q == sym

getTransition :: State -> Symbol -> [Transition5] -> Maybe Transition5
getTransition state sym trs =   if null validCases
                                then Nothing 
                                else Just $ head validCases
    where 
         validCases = filter (\z -> hasValidCondition z state sym) trs