{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Turing.Machine.RevMachine (RevMachine (..)) where

import Turing.Basic.State
import Turing.Machine.Machine
import Turing.Tape.TripleTape
import Turing.Transition.Transition4
import Utils

data RevMachine = RevTm
  { tapes :: TripleTape,
    transitions :: [Transition4],
    currentState :: State,
    acceptState :: State,
    counter :: Int,
    alphabet :: [Symbol],
    halt :: Bool
  }

instance Show RevMachine where
  show tm =
      show t1 ++ " "
      ++ show t2 ++ " "
      ++ show t3 ++ " "
      ++ currentState tm
    where
      (t1, t2, t3) = tapes tm


instance TuringMachine Transition4 TripleTape RevMachine where
  mkTm tps trs st acc alp = RevTm tps trs st acc 0 alp False

  tmHalt = halt
  tmSetHalt tm h = tm {halt = h}

  tmCurrentSt = currentState
  tmAcceptSt = acceptState

  showDefinition tm = align (map show (transitions tm))
  showStats tm = "Steps: " ++ show (counter tm) ++ "\nAccepted: " ++ show (tmAccepted tm)
  
  tmNextTr tm = getTransition (tmCurrentSt tm) readSymbs (transitions tm)
    where readSymbs = tape3Read (tapes tm)

  tmPerformTr tm tr
    = tm { tapes = newTapes,
           currentState = to tr,
           counter      = counter tm + 1 }
    where
      newTapes = tape3Perform (tapes tm) (outAct tr)