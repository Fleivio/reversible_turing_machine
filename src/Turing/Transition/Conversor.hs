{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}
module Turing.Transition.Conversor (toQuadruple, genComputeTransitions, genOutputCopyTransitions) where

import Turing.Basic.State
import Turing.Basic.Symbol
import Turing.Tape.Basic.Action
import Turing.Tape.Basic.Direction
import Turing.Transition.Transition4
import Turing.Transition.Transition5 qualified as T5

toQuadruple :: T5.Transition5 -> (Transition4, Transition4)
toQuadruple q = (first, second)
	where
		interState = stCombine (T5.from q) (T5.to q) (T5.rSym q)
		first =
			Transition4
				{ from   = T5.from q,
					inAct  = (Readt (T5.rSym q), Bar, Readt memptySymbol),
					to     = interState,
					outAct = (Writet (T5.wSym q), Shift R, Writet memptySymbol)
				}
		second =
			Transition4
				{ from   = interState,
					inAct  = (Bar, Readt memptySymbol, Bar),
					to     = T5.to q,
					outAct = (Shift (T5.dir q), Writet (stGetName interState), Shift S)
				}

genComputeTransitions :: [T5.Transition5] -> [Transition4]
genComputeTransitions trs5 = concatMap (\(x, y) -> [x, y]) tuples
	where
		tuples = map toQuadruple trs5

genOutputCopyTransitions :: State -> [Symbol] -> ([Transition4], State)
genOutputCopyTransitions af alphabet =
	([afb1l, b1lb1, b1b2l, b2lb2, b2cf] ++ b1b1l ++ b2b2l, cf)
	where
		alp = (filter (/= memptySymbol) alphabet)
		b1l = State "b1l"
		b1  = State "b1"
		b2l = State "b2l"
		b2  = State "b2"
		cf  = State "cf"
		afb1l =
			Transition4
				{ from   = af,
					inAct  = (Readt memptySymbol, Bar, Readt memptySymbol),
					to     = b1l,
					outAct = (Writet memptySymbol, Shift S, Writet memptySymbol)
				}
		b1lb1 =
			Transition4
				{ from   = b1l,
					inAct  = (Bar, Bar, Bar),
					to     = b1,
					outAct = (Shift R, Shift S, Shift R)
				}
		b1b1l =
			flip
				map
				alp
				( \x ->
						Transition4
							{ from   = b1,
								inAct  = (Readt x, Bar, Readt memptySymbol),
								to     = b1l,
								outAct = (Writet x, Shift S, Writet x)
							}
				)
		b1b2l =
			Transition4
				{ from   = b1,
					inAct  = (Readt memptySymbol, Bar, Readt memptySymbol),
					to     = b2l,
					outAct = (Writet memptySymbol, Shift S, Writet memptySymbol)
				}
		b2lb2 =
			Transition4
				{ from   = b2l,
					inAct  = (Bar, Bar, Bar),
					to     = b2,
					outAct = (Shift R, Shift S, Shift R)
				}
		b2b2l =
			flip
				map
				alp
				( \x ->
						Transition4
							{ from   = b2,
								inAct  = (Readt x, Bar, Readt x),
								to     = b2l,
								outAct = (Writet x, Shift S, Writet x)
							}
				)
		b2cf =
			Transition4
				{ from   = b2,
					inAct  = (Readt memptySymbol, Bar, Readt memptySymbol),
					to     = cf,
					outAct = (Writet memptySymbol, Shift S, Writet memptySymbol)
				}

