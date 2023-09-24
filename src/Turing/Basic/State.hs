module Turing.Basic.State (State, Symbol, stCombine, inverseState) where

import Turing.Basic.Symbol (Symbol)

type State = Symbol

stCombine :: State -> State -> Symbol -> State
stCombine s1 s2 s = s1 ++ s2 ++ s

inverseState :: State -> State
inverseState s = "i" ++ s
