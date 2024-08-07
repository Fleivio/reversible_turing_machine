{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Turing.Machine.Machine (TuringMachine (..), module Control.Monad.Writer) where

import Turing.Basic.State

import Control.Monad.Writer

class (Show tm) => TuringMachine tr tp tm | tm -> tr, tm -> tp where
  mkTm :: tp -> [tr] -> State -> State -> [Symbol] -> tm

  tmHalt :: tm -> Bool
  tmSetHalt :: tm -> Bool -> tm

  tmCurrentSt :: tm -> State
  tmAcceptSt :: tm -> State
  showDefinition :: tm -> String
  showStats :: tm -> String

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

  tmShowRun :: tm -> Writer String tm
  tmShowRun tm' | tmHalt tm' = pure tm'
  tmShowRun tm' = tell (show tm' <> "\n") >> tmShowRun (tmStep tm')
