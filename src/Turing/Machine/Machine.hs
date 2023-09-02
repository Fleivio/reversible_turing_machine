module Turing.Machine.Machine(TuringMachine(..)) where

class TuringMachine tm where
    tmHalt :: tm -> Bool
    tmStep :: tm -> tm

    tmRun :: tm -> tm
    tmRun tm = if tmHalt tm 
               then tm 
               else tmRun (tmStep tm)
