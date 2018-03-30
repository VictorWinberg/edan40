module Ex1
    ( maxi
    , recsumsq
    , mapsumsq
    , rechanoi
    , maphanoi
    , smallestFactor
    , recnumFactors
    , mapnumFactors
    ) where

maxi :: Integer -> Integer -> Integer
maxi x y
  | x > y = x
  | otherwise = y

-- recursion
recsumsq :: Integer -> Integer
recsumsq 0 = 0
recsumsq n
  | n > 0 = n * n + recsumsq (n - 1)
  | otherwise = -1

-- mapping
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

-- mapping
maphanoi :: Integer -> Integer
maphanoi n
  | n > 0 = sum $ map poweroftwo [0..n-1]
  | otherwise = -1
    where poweroftwo x = 2 ^ x

smallestFactor :: Integer -> Integer
smallestFactor n
  | n > 1 = nextFactor 2 n
  | n > 0 = 1
  | otherwise = -1

nextFactor :: Integer -> Integer -> Integer
nextFactor k n
  | k > n = n
  | mod n k == 0 = k
  | otherwise = nextFactor (k + 1) n

-- recursion
recnumFactors :: Integer -> Integer
recnumFactors n
  | n > 1 = (+) 1 $ recnumFactors $ div n $ smallestFactor n
  | n > 0 = 0
  | otherwise = -1

-- mapping
mapnumFactors :: Integer -> Integer
mapnumFactors n
  | n > 1 = maxi 1 $ flip (-) 1 $ toInteger $ length $ removeDups $ map (flip nextFactor n) [2..n]
  | n > 0 = 0
  | otherwise = -1

removeDups :: [Integer] -> [Integer]
removeDups [] = []
removeDups (x:xs)
  | elem x xs = removeDups xs
  | otherwise = x : removeDups xs
