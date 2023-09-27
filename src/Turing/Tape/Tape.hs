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
  show = filter (/= '"') . show . content

mkTape :: Symbol -> Tape
mkTape = Tape [] []

mkTapeFromList :: Symbol -> [Symbol] -> Tape
mkTapeFromList b l = Tape [] l b

tapeRead :: Tape -> Symbol
tapeRead (Tape _ [] b)      = b
tapeRead (Tape _ (x : _) _) = x

tapeWrite :: Tape -> Symbol -> Tape
tapeWrite (Tape l r b) v
  = cleanTape $ case r of
      [] -> Tape l [v] b
      _  -> Tape l (v : tail r) b

tapeShift :: Tape -> Direction -> Tape
tapeShift tp Z = tp
tapeShift (Tape l1 r1 b) d 
  = cleanTape $ case (l1, r1, d) of
      ([]  , rs  , L) -> Tape [] (b : rs) b
      (ls  , []  , R) -> Tape (b : ls) [] b
      (l:ls, rs  , L) -> Tape ls (l : rs) b
      (ls  , r:rs, R) -> Tape (r : ls) rs b
  
cleanTape :: Tape -> Tape
cleanTape t@(Tape l r b) 
  = t { left = clean l,
        right = clean r }
  where
    clean list
      | all (== b) list = []
      | otherwise = list 

content :: Tape -> [Symbol]
content (Tape l r _) = reverse l ++ r