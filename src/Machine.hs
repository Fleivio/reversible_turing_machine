module Machine() where

import Turing.State
import Turing.Symbol
import Turing.Action
import qualified Turing.Quadruple as Qa
import qualified Turing.Quintuple as Qi


toQuadrupleInCons :: Qi.Quintuple -> (Qa.Quadruple, Qa.Quadruple)
toQuadrupleInCons q = (first, second)
    where 
        interState = Qi.intermediateState q
        first = Qa.Quadruple (Qi.from q)
                             interState 
                             (Read (Qi.rSym q), Bar, Read emptySymbol)
                             (Write (Qi.wSym q), Shift R, Write emptySymbol)
        second = Qa.Quadruple interState 
                              (Qi.to q)
                              (Bar, Read emptySymbol, Bar)
                              (Shift (Qi.dir q), Write (stGetName interState), Shift S)    
