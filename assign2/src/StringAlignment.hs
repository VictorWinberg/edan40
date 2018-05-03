module StringAlignment where

type AlignmentType = (String,String)

similarityScore :: String -> String -> Int
similarityScore [] _ = 0
similarityScore _ [] = 0
similarityScore (x:xs) (y:ys)
  | x == '-' || y == '-' = (-2) + similarityScore xs ys
  | x == y = 1 + similarityScore xs ys
  | otherwise = (-1) + similarityScore xs ys

{-attachHeads is adding h1 and h2 first in  each of the element in one duples.
The funtion does this for all the duples in the list.
h1 first in the first of the two duples and h2 firts in the second.
Examlpe:
attachHeads a e [(bcd, fgh), (jkl, nop)]
gives us [(abcd, efgh), (ajkl, enop)]-}
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = []
maximaBy f xs = filter (\x -> f x == maxi) xs
  where maxi = maximum $ map f xs


-- which returns a list of all optimal alignments between string1 and string2.
optAlignments :: String -> String -> [AlignmentType]
optAlignments string1 string2 = undefined
