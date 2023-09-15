module Turing.Tape.RevTape (Tape, mkTape, mkTapeFromList, tapePerformOutAction, tapePerformInAction) where

import Turing.Basic.Action
import Turing.Tape.Tape

tapePerformInAction :: Tape -> InAction
tapePerformInAction = Readt . tapeRead 

tapePerformOutAction :: Tape -> OutAction -> Tape
tapePerformOutAction tp (Writet a) = tapeWrite tp a
tapePerformOutAction tp (Shift d)  = tapeShift tp d