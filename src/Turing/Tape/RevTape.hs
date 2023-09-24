module Turing.Tape.RevTape (Tape, mkTape, mkTapeFromList, tapePerformOutAction, tapePerformInAction) where

import Turing.Basic.Action
import Turing.Tape.Tape

tapePerformInAction :: Tape -> InAction
tapePerformInAction = Rd . tapeRead

tapePerformOutAction :: Tape -> OutAction -> Tape
tapePerformOutAction tp (Wrt a) = tapeWrite tp a
tapePerformOutAction tp (Sft d)  = tapeShift tp d