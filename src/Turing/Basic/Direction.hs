module Turing.Basic.Direction (Direction (..), revDir) where

data Direction = R | L | S deriving (Eq, Show)

revDir :: Direction -> Direction
revDir d = case d of
  R -> L
  L -> R
  S -> S