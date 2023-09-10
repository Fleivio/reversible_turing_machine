module Turing.Transition.Conversor(toQuadruple, genComputeTransitions, genOutputCopyTransitions) where

import qualified Turing.Transition.Transition4 as T4
import qualified Turing.Transition.Transition5 as T5
import Turing.Tape.Basic.Action
import Turing.Basic.Symbol  
import Turing.Tape.Basic.Direction
import Turing.Basic.State

toQuadruple :: T5.Transition5 -> (T4.Transition4, T4.Transition4)
toQuadruple q = (first, second)
    where
        interState = stCombine (T5.from q) (T5.to q) (T5.rSym q)
        first = T4.Transition4 (T5.from q)
                               (Readt (T5.rSym q), Bar, Readt memptySymbol)
                               interState  
                               (Writet (T5.wSym q), Shift R, Writet memptySymbol)
        second = T4.Transition4 interState 
                                (Bar, Readt memptySymbol, Bar)
                                (T5.to q)
                                (Shift (T5.dir q), Writet (stGetName interState), Shift S)    

genComputeTransitions :: [T5.Transition5] -> [T4.Transition4]
genComputeTransitions trs5 = concatMap (\(x, y) -> [x, y]) tuples
    where tuples = map toQuadruple trs5

genOutputCopyTransitions :: State -> [Symbol] -> ([T4.Transition4], State)
genOutputCopyTransitions interState alphabet = (,) ([afb1', b1'b1, b1b2', b2'b2, b2cf] ++ b1b1' ++ b2b2') cf
    where 
        b1' = State "b1'"
        b1 = State "b1"
        b2' = State "b2'"
        b2 = State "b2"
        cf = State "cf"
        afb1' = T4.Transition4 interState ( Readt memptySymbol, Bar, Readt memptySymbol )
                              b1' ( Writet memptySymbol, Shift S, Writet memptySymbol )
        b1'b1 = T4.Transition4 b1' ( Bar, Bar, Bar)
                               b1 ( Shift R, Shift S, Shift R )
        b1b1' = map (\x -> T4.Transition4 b1 ( Readt x, Bar, Readt memptySymbol )
                                          b1' ( Writet x, Shift S, Writet x)) alphabet
        b1b2' = T4.Transition4 b1 (Readt memptySymbol, Bar, Readt memptySymbol)
                               b2' (Writet memptySymbol, Shift S, Writet memptySymbol)
        b2'b2 = T4.Transition4 b2' (Bar, Bar, Bar) b2 (Shift L, Shift S, Shift L)
        b2b2' = map (\x -> T4.Transition4 b2 (Readt x, Bar, Readt x) 
                                          b2' (Writet x, Shift S, Writet x)) alphabet
        b2cf = T4.Transition4 b2 (Readt memptySymbol, Bar, Readt memptySymbol)
                              cf (Writet memptySymbol, Shift S, Writet memptySymbol)


--toQuintuple :: (T4.Transition4, T4.Transition4) -> T5.Transition5
