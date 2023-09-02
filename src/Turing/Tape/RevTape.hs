module Turing.Tape.RevTape (GenericTape(..), mkTape, mkTapeFromList, tapePerformOutAction, tapePerformInAction) where

import Turing.Tape.Basic.Action
import Turing.Tape.Basic.GenericTape

tapePerformInAction :: GenericTape a -> InAction a 
tapePerformInAction tp = Readt $ tapeRead tp 

tapePerformOutAction :: GenericTape a -> OutAction a -> GenericTape a
tapePerformOutAction tp (Writet a) = tapeWrite tp a
tapePerformOutAction tp (Shift d) = tapeShift tp d