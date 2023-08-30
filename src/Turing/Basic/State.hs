module Turing.Basic.State(State(..), stGetName, stCombine) where

import Turing.Basic.Symbol (Symbol)

newtype State = State Symbol deriving (Eq, Show)

stGetName :: State -> Symbol
stGetName (State s) = s

stCombine :: State -> State -> State
stCombine s1 s2 = State $ stGetName s1 <> stGetName s2