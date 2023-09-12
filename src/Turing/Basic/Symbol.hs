module Turing.Basic.Symbol (Symbol, memptySymbol) where

-- must implement Monoid contatenation and empty
type Symbol = String

memptySymbol :: Symbol
memptySymbol = "B"
