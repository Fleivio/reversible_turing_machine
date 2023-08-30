module Turing.Tape.TripleTape(TripleInAction(..), TripleOutAction(..), TripleTape(..), mkTape3, mkTape3FromLists, tape3PerformInAction, tape3PerformOutAction) where

import Turing.Tape.RevTape 
import Turing.Tape.Basic.Action

data TripleTape a = TripleTp (Tape a) (Tape a) (Tape a) deriving (Eq, Show)

data TripleInAction a = TripleInAction (InAction a) (InAction a) (InAction a) deriving (Eq, Show)

data TripleOutAction a = TripleOutAction (OutAction a) (OutAction a) (OutAction a) deriving (Eq, Show)

mkTape3 :: (a,a,a) -> TripleTape a
mkTape3 (a,b,c) = TripleTp (mkTape a) (mkTape b) (mkTape c)

mkTape3FromLists :: (a,a,a) -> ([a], [a], [a]) -> TripleTape a
mkTape3FromLists (a,b,c) (l1,l2,l3) = TripleTp (mkTapeFromList a l1) (mkTapeFromList b l2) (mkTapeFromList c l3)

tape3PerformInAction :: TripleTape a -> TripleInAction a -> (a,a,a)
tape3PerformInAction (TripleTp l m r) (TripleInAction ia1 ia2 ia3) 
    = (tapePerformInAction l ia1, tapePerformInAction m ia2, tapePerformInAction r ia3)

tape3PerformOutAction :: TripleTape a -> TripleOutAction a -> TripleTape a
tape3PerformOutAction (TripleTp l m r) (TripleOutAction oa1 oa2 oa3) 
    = TripleTp (tapePerformOutAction l oa1) (tapePerformOutAction m oa2) (tapePerformOutAction r oa3)