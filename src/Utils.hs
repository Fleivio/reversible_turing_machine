module Utils((<+>), zipTrip, replace, splitBy, align) where

import Data.List (transpose)

(<+>) :: (a -> b) -> (a, a, a) -> (b, b, b)
f <+> (a, b, c) = (f a, f b, f c)

zipTrip :: (a,a,a) -> (b,b,b) -> ((a,b), (a,b), (a,b))
zipTrip (a1, a2, a3) (b1, b2, b3) = ((a1, b1), (a2, b2), (a3, b3))

replace :: (Eq a) => a -> a -> [a] -> [a]
replace something replacement =
  map (\x -> if x == something then replacement else x)

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy by =
  foldr (\x acc -> if by == x then [] : acc else (x : head acc) : tail acc) [[]]

splitF :: (a -> Bool) -> [a] -> [[a]]
splitF f =
  foldr (\x acc -> if f x then [x] : acc else (x : head acc) : tail acc) [[]]

align :: [String] -> String
align = addSpaces . unlines . allsTr . trimSpaces
  where
    addSpaces str = case str of 
      (a : xs) | isDelim a -> a : ' ' : addSpaces xs
      (a : xs) -> a : addSpaces xs
      [] -> []
    trimSpaces = map $ filter (/= ' ')
    isDelim x = x `elem` [')', '(', ',']
    allsTr a = map concat . transpose . map aligAll . transpose $ map splits a
      where
        aligAll strs' = map (`fillRest` maxSize) strs' 
          where
            maxSize = maximum (map length strs')
            fillRest str' n
              | length str' <= 1 = str' ++ replicate (n - length str') ' '
              | otherwise = init str' ++ replicate (n - length str') ' ' ++ [last str']
        splits = splitF isDelim
          

