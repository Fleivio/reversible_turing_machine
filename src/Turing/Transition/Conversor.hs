module Turing.Transition.Conversor(toQuadruple) where

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
                               (Readt (T5.rSym q), Bar, Readt (mempty :: Symbol))
                               interState  
                               (Writet (T5.wSym q), Shift R, Writet (mempty :: Symbol))
        second = T4.Transition4 interState 
                                (Bar, Readt (mempty :: Symbol), Bar)
                                (T5.to q)
                                (Shift (T5.dir q), Writet (stGetName interState), Shift S)    

--toQuintuple :: (T4.Transition4, T4.Transition4) -> T5.Transition5
