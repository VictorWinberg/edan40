module Ex1
    ( maxi
    , recsumsq
    , mapsumsq
    ) where

maxi x y
  | x > y = x
  | otherwise = y

-- recursion
recsumsq :: Integer -> Integer
recsumsq 0 = 0
recsumsq n = n * n + recsumsq (n - 1)

-- map
mapsumsq :: Integer -> Integer
mapsumsq n = sum $ map sq [1..n]
  where sq x = x * x

-- hanoi n
