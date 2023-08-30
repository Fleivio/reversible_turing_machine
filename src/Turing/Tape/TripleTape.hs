module Turing.Tape.TripleTape(TripleInAction, TripleOutAction, TripleTape, mkTape3, mkTape3FromLists, tape3PerformInAction, tape3PerformOutAction) where

import Turing.Tape.RevTape 
import Turing.Tape.Basic.Action
import Turing.Basic.Symbol

type TripleTape =  (Tape Symbol, Tape Symbol, Tape Symbol)

type TripleInAction = (InAction Symbol, InAction Symbol, InAction Symbol)

type TripleOutAction = (OutAction Symbol, OutAction Symbol, OutAction Symbol)

type TripleSymbol = (Symbol, Symbol, Symbol)

mkTape3 :: TripleSymbol -> TripleTape
mkTape3 (a,b,c) = (,,) (mkTape a) (mkTape b) (mkTape c)

mkTape3FromLists :: TripleSymbol -> ([Symbol], [Symbol], [Symbol]) -> TripleTape
mkTape3FromLists (a,b,c) (l1,l2,l3) = (,,) (mkTapeFromList a l1) (mkTapeFromList b l2) (mkTapeFromList c l3)

tape3PerformInAction :: TripleTape -> TripleInAction -> TripleSymbol
tape3PerformInAction (l, m, r) (ia1, ia2, ia3) 
    = (tapePerformInAction l ia1, tapePerformInAction m ia2, tapePerformInAction r ia3)

tape3PerformOutAction :: TripleTape -> TripleOutAction -> TripleTape
tape3PerformOutAction (l, m, r) (oa1, oa2, oa3) 
    = (,,) (tapePerformOutAction l oa1) (tapePerformOutAction m oa2) (tapePerformOutAction r oa3)