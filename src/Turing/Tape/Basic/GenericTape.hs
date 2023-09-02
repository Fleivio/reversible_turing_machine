module Turing.Tape.Basic.GenericTape(GenericTape(..), mkTape, content, mkTapeFromList, tapeRead, tapeWrite, tapeShift) where

import Turing.Tape.Basic.Direction

data GenericTape a = GenericTape {
        left  :: [a],
        right :: [a],
        basic :: a
    } deriving (Eq)

instance Show a => Show (GenericTape a) where
    show t = show (reverse (left t)) ++ show (right t)

mkTape :: a -> GenericTape a
mkTape = GenericTape [] []

mkTapeFromList :: a -> [a] -> GenericTape a
mkTapeFromList b l = GenericTape [] l b 

tapeRead :: GenericTape a -> a
tapeRead (GenericTape _ [] b) = b
tapeRead (GenericTape _ (x:_) _) = x

tapeWrite :: GenericTape a -> a -> GenericTape a
tapeWrite (GenericTape l [] b) v = GenericTape l [v] b
tapeWrite (GenericTape l (_:r) b) v = GenericTape l (v:r) b

tapeShift :: GenericTape a -> Direction -> GenericTape a
tapeShift (GenericTape [] rs b) L = GenericTape [] (b:rs) b
tapeShift (GenericTape ls [] b) R = GenericTape (b:ls) [] b
tapeShift (GenericTape (l:ls) rs b) L = GenericTape ls (l:rs) b
tapeShift (GenericTape ls (r:rs) b) R = GenericTape (r:ls) rs b
tapeShift tp S = tp

content :: GenericTape a -> [a]
content (GenericTape l r b) = [b] ++ reverse l ++ r ++ [b]