module Turing.Tape.RevTape (Tape, mkTape, mkTapeFromList, tapePerformOutAction, tapePerformInAction) where

import Turing.Basic.Symbol
import Turing.Tape.Basic.Action
import Turing.Tape.Tape

tapePerformInAction :: Tape -> InAction Symbol
tapePerformInAction tp = Readt $ tapeRead tp

tapePerformOutAction :: Tape -> OutAction Symbol -> Tape
tapePerformOutAction tp (Writet a) = tapeWrite tp a
tapePerformOutAction tp (Shift d) = tapeShift tp d