{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
module Turing.Transition.Transition(Transition(..)) where 

import Turing.Basic.State
import Data.List (find)

class Transition tr inAct | tr -> inAct where
    hasValidCondition :: tr -> State -> inAct -> Bool 

    from :: tr -> State
    to :: tr -> State

    getTransition :: State -> inAct -> [tr] -> Maybe tr
    getTransition state symb3 = find (\x -> hasValidCondition x state symb3)

    getTransitionThatGoesTo :: State -> [tr] -> [tr]
    getTransitionThatGoesTo state = filter (\x -> to x == state)