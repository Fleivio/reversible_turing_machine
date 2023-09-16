module Turing.Machine.Machine (TuringMachine (..)) where

import Turing.Basic.State

class (Show tm) => TuringMachine tm where
  tmHalt :: tm -> Bool
  tmStep :: tm -> tm

  tmCurrentSt :: tm -> State
  tmAcceptSt :: tm -> State

  tmAccepted :: tm -> Bool
  tmAccepted tm = tmCurrentSt tm == tmAcceptSt tm && tmHalt tm

  tmRun :: tm -> tm
  tmRun tm | tmHalt tm = tm
  tmRun tm = tmRun . tmStep $ tm

  tmShowRun :: tm -> (String, tm)
  tmShowRun tm = tmShowRun' tm ""
    where
      tmShowRun' tm' str | tmHalt tm' = (str ++ "\n" ++ show tm', tm')
      tmShowRun' tm' str = (str ++ "\n" ++ st2, tm2)
        where 
          (st2, tm2) = tmShowRun' (tmStep tm') (show tm')

  showDefinition :: tm -> String
