module Turing.Tape.RevTape (Tape(..), mkTape, mkTapeFromList, tapePerformOutAction, tapePerformInAction) where

import Turing.Tape.Basic.Action
import Turing.Tape.Basic.Tape

tapePerformInAction :: Tape a -> InAction a -> a
tapePerformInAction tp _ = tapeRead tp

tapePerformOutAction :: Tape a -> OutAction a -> Tape a
tapePerformOutAction tp (Writet a) = tapeWrite tp a
tapePerformOutAction tp (Shift d) = tapeShift tp d