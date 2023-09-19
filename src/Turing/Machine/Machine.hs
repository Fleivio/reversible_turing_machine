{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
module Turing.Machine.Machine (TuringMachine (..)) where

import Turing.Basic.State

class (Show tm) => TuringMachine tr tp tm | tm -> tr, tm -> tp where
  mkTm :: tp -> [tr] -> State -> State -> [Symbol] -> tm

  tmHalt :: tm -> Bool
  tmSetHalt :: tm -> Bool -> tm

  tmCurrentSt :: tm -> State
  tmAcceptSt :: tm -> State

  showDefinition :: tm -> String

  tmNextTr :: tm -> Maybe tr
  tmPerformTr :: tm -> tr -> tm

  tmStep :: tm -> tm
  tmStep tm =
      case tmNextTr tm of
        Nothing -> tmSetHalt tm True
        Just tr -> tmPerformTr tm tr

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

