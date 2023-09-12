module Turing.Basic.Direction (Direction (..), revDir) where

data Direction = R | L | S deriving (Eq)

instance Show Direction where
  show R = "+"
  show L = "-"
  show S = "0"

revDir :: Direction -> Direction
revDir d = case d of
  R -> L
  L -> R
  S -> S