module Turing.Tape.TripleTape (TripleInAction, TripleOutAction, TripleTape, mkTape3, mkTape3FromLists, tape3Read, tape3Perform) where

import Turing.Basic.Action
import Turing.Tape.RevTape

type TripleTape = (Tape, Tape, Tape)

type TripleInAction = (InAction, InAction, InAction)

type TripleOutAction = (OutAction, OutAction, OutAction)

type TripleSymbol = (Symbol, Symbol, Symbol)

mkTape3 :: TripleSymbol -> TripleTape
mkTape3 symbs = mkTape <+> symbs

mkTape3FromLists :: TripleSymbol -> ([Symbol], [Symbol], [Symbol]) -> TripleTape
mkTape3FromLists symbs tapes = 
  uncurry mkTapeFromList <+> zipTrip symbs  tapes

tape3Read :: TripleTape -> TripleInAction
tape3Read tapes = tapePerformInAction <+> tapes

tape3Perform :: TripleTape -> TripleOutAction -> TripleTape
tape3Perform tapes actions =
  uncurry tapePerformOutAction <+> zipTrip tapes actions

(<+>) :: (a -> b) -> (a, a, a) -> (b, b, b)
f <+> (a, b, c) = (f a, f b, f c)

zipTrip :: (a,a,a) -> (b,b,b) -> ((a,b), (a,b), (a,b))
zipTrip (a1, a2, a3) (b1, b2, b3) = ((a1, b1), (a2, b2), (a3, b3))
