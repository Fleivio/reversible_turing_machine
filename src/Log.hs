module Log(Log(..)) where

data Log a = Log String a
    deriving (Show, Eq)

instance Functor Log where
    fmap f (Log s c) = Log s (f c)

instance Applicative Log where
    pure = Log ""
    Log _ f <*> (Log s c) = Log s (f c)

instance Monad Log where
    return = pure
    (Log s c) >>= f = let (Log s1 c1) = f c
                      in Log (s ++ "\n" ++ s1) c1 