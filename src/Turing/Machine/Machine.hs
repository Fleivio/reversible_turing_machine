module Turing.Machine.Machine(TuringMachine(..)) where

class TuringMachine tm where
    tmHalt :: tm -> Bool
    tmStep :: tm -> tm

    tmRun :: tm -> tm
    tmRun tm | tmHalt tm = tm
    tmRun tm = tmRun . tmStep $ tm
