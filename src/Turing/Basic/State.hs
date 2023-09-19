module Turing.Basic.State (State (..), Symbol, stGetName, stCombine, inverseState) where

import Turing.Basic.Symbol (Symbol)

newtype State = State Symbol deriving (Eq)

instance Show State where
  show (State s) = s

stGetName :: State -> Symbol
stGetName (State s) = s

stCombine :: State -> State -> Symbol -> State
stCombine s1 s2 s =
  State $
    stGetName s1
      ++ stGetName s2
      ++ s

inverseState :: State -> State
inverseState s = State $ "i" ++ stGetName s
