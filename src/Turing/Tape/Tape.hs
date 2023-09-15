module Turing.Tape.Tape (Tape (..), mkTape, content, mkTapeFromList, tapeRead, tapeWrite, tapeShift) where
  
import Turing.Basic.Direction
import Turing.Basic.Symbol

data Tape = Tape
  { left :: [Symbol],
    right :: [Symbol],
    basic :: Symbol
  }
  deriving (Eq)

instance Show Tape where
  show t = show (reverse (left t)) ++ show (right t)

mkTape :: Symbol -> Tape
mkTape = Tape [] []

mkTapeFromList :: Symbol -> [Symbol] -> Tape
mkTapeFromList b l = Tape [] l b

tapeRead :: Tape -> Symbol
tapeRead (Tape _ [] b)      = b
tapeRead (Tape _ (x : _) _) = x

tapeWrite :: Tape -> Symbol -> Tape
tapeWrite (Tape l [] b) v      = Tape l [v] b
tapeWrite (Tape l (_ : r) b) v = Tape l (v : r) b

tapeShift :: Tape -> Direction -> Tape
tapeShift (Tape [] rs b) L       = Tape [] (b : rs) b
tapeShift (Tape ls [] b) R       = Tape (b : ls) [] b
tapeShift (Tape (l : ls) rs b) L = Tape ls (l : rs) b
tapeShift (Tape ls (r : rs) b) R = Tape (r : ls) rs b
tapeShift tp S = tp

content :: Tape -> [Symbol]
content (Tape l r b) = [b] ++ reverse l ++ r ++ [b]