module Turing.Tape.RevTape (Tape, mkTape, mkTapeFromList, tapePerformOutAction, tapePerformInAction) where

import Turing.Tape.Basic.Action
import Turing.Tape.Tape
import Turing.Basic.Symbol

tapePerformInAction :: Tape -> InAction Symbol
tapePerformInAction tp = Readt $ tapeRead tp 

tapePerformOutAction :: Tape -> OutAction Symbol -> Tape
tapePerformOutAction tp (Writet a) = tapeWrite tp a
tapePerformOutAction tp (Shift d) = tapeShift tp d