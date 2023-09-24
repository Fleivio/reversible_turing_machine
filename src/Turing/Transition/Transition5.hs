{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Turing.Transition.Transition5 (Transition5 (..), Transition (..)) where

import Turing.Transition.Transition
import Turing.Basic.Direction (Direction)
import Turing.Basic.State

data Transition5 = Tr5
  { from5 :: State,
    rSym :: Symbol,
    to5 :: State,
    wSym :: Symbol,
    dir :: Direction
  }
  deriving (Eq)

instance Show Transition5 where
  show (Tr5 f rs t ws d) = leftSide ++ alignment ++ " -> " ++ rightSide
    where
      alignment = replicate (8 - length leftSide) ' '
      leftSide  = "(" ++ f ++ ", " ++ show rs ++ ")"
      rightSide = "(" ++ t ++ ", " ++ show ws ++ ", " ++ show d ++ ")"

instance Transition Transition5 Symbol where
  hasValidCondition tr state symb = from tr == state && rSym tr == symb

  to = to5
  from = from5