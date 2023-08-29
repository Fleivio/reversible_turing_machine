{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib
    ( 
    Quadruple(..)) where

type Symbol = String

emptySymbol :: Symbol
emptySymbol = ""

data Direction = R | L | S

data InAction = Read Symbol | Bar

instance Eq InAction where
    (Read s1) == (Read s2) = s1 == s2
    Bar == _ = True
    _ == Bar = True

data OutAction = Write Symbol | Shift Direction

newtype State = State String deriving Eq

type TripleInAction = (InAction, InAction, InAction)
type TripleOutAction = (OutAction, OutAction, OutAction)

stGetName :: State -> String
stGetName (State s) = s

data Quadruple = Quadruple {
        from   :: State,
        to     :: State,
        inAct  :: TripleInAction,
        outAct :: TripleOutAction
    }

data Quintuple = Quintuple {
        from' :: State,
        to'   :: State,
        rSym  :: Symbol,
        wSym  :: Symbol,
        dir   :: Direction
    }

toQuadrupleInCons :: Quintuple -> (Quadruple, Quadruple)
toQuadrupleInCons q = (first, second)
    where 
        first = Quadruple (from' q) interState (Read (rSym q), Bar, Read emptySymbol)
                                               (Write (wSym q), Shift R, Write emptySymbol)
        second = Quadruple interState (to' q) (Bar, Read emptySymbol, Bar)
                                              (Shift (dir q), Write (stGetName interState) , Shift S)
        interState = intermediateState q    

getQuadruple :: State -> TripleInAction -> [Quadruple] -> Quadruple
getQuadruple state tia = head . filter (\q -> from q == state && inAct q == tia)

intermediateState :: Quintuple -> State
intermediateState q = State (a ++ b)
    where
        (State a) = from' q
        (State b) = to' q



-- e1 (condicao) -> e2 (ação)
-- e1 (read) -> e2 (write)
-- e1 (/) -> e2 (shift)