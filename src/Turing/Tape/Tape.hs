module Turing.Tape.Tape (Tape, mkTape, mkTapeFromList, tapeRead, tapeWrite) where

import Turing.Tape.Basic.GenericTape
import Turing.Basic.Symbol

type Tape = GenericTape Symbol