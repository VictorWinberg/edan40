module Ex1
    ( maxi
    , recsumsq
    , mapsumsq
    , rechanoi
    , maphanoi
    ) where

maxi x y
  | x > y = x
  | otherwise = y

-- recursion
recsumsq :: Integer -> Integer
recsumsq 0 = 0
recsumsq n
  | n > 0 = n * n + recsumsq (n - 1)
  | otherwise = -1

-- map
mapsumsq :: Integer -> Integer
mapsumsq n
  | n >= 0 = sum $ map sq [1..n]
  | otherwise = -1
    where sq x = x * x

-- recursion
rechanoi :: Integer -> Integer
rechanoi 1 = 1
rechanoi n
  | n > 1 = 2 ^ (n - 1) + rechanoi (n - 1)
  | otherwise = -1

-- map
maphanoi :: Integer -> Integer
maphanoi n
  | n > 0 = sum $ map poweroftwo [0..n-1]
  | otherwise = -1
    where poweroftwo x = 2 ^ x
