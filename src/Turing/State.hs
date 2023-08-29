module Turing.State(State(..), stGetName, stCombine) where

newtype State = State String deriving Eq

stGetName :: State -> String
stGetName (State s) = s

stCombine :: State -> State -> State
stCombine s1 s2 = State $ stGetName s1 ++ "." ++ stGetName s2