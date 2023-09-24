{-# LANGUAGE MultiParamTypeClasses #-}

module Turing.Machine.ClassicMachine (ClassicMachine (..)) where

import Turing.Basic.State
import Turing.Machine.Machine
import Turing.Tape.Tape
import Turing.Transition.Transition5
import Utils

data ClassicMachine = ClassTm
  { tape :: Tape,
    transitions :: [Transition5],
    currentState :: State,
    acceptState :: State,
    counter :: Int,
    alphabet :: [Symbol],
    halt :: Bool
  }

instance Show ClassicMachine where
  show tm =
    show (tape tm) ++ " " ++ currentState tm

instance TuringMachine Transition5 Tape ClassicMachine where
  mkTm tp trs st acc alp = ClassTm tp trs st acc 0 alp False

  tmHalt = halt
  tmSetHalt tm h = tm {halt = h}

  tmCurrentSt = currentState
  tmAcceptSt = acceptState

  showDefinition tm = align (map show (transitions tm))
  showStats tm = "Steps: " ++ show (counter tm) ++ "\nAccepted: " ++ show (tmAccepted tm)

  tmNextTr tm = getTransition (tmCurrentSt tm) readSymbs (transitions tm)
    where readSymbs = tapeRead (tape tm)

  tmPerformTr tm tr =
    tm
      { tape = newTape,
        currentState = to tr,
        counter = counter tm + 1
      }
    where
      newTape = tapeShift (tapeWrite (tape tm) (wSym tr)) (dir tr)