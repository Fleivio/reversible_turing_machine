module Turing.Transition.Conversor() where

-- import qualified Turing.Transition.Transition4 as T4
-- import qualified Turing.Transition.Transition5 as T5
-- import Turing.Tape.Basic.Action
-- import Turing.Basic.Symbol  
-- import Turing.Tape.TripleTape
-- import Turing.Tape.Basic.Direction

-- toQuadrupleInCons :: T5.Transition5 -> (T4.Transition4, T4.Transition4)
-- toQuadrupleInCons q = (first, second)
--     where 
--         interState = T5.intermediateState q
--         first = T4.Transition4 (T5.from q)
--                                (TripleInAction (Readt (T5.rSym q)) Bar (Readt (mempty :: Symbol)))
--                                interState  
--                                (TripleOutAction (Writet (T5.wSym q)) (Shift R) (Writet (mempty :: Symbol)))
--         second = T4.Transition4 interState 
--                                 (TripleInAction (Bar, Readt (mempty :: Symbol), Bar))
--                                 (T5.to q)
--                                 (Shift (T5.dir q), Writet (stGetName interState), Shift S)    
