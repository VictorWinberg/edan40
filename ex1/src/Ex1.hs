module Ex1
    ( maxi
    , recsumsq
    , mapsumsq
    , rechanoi
    , maphanoi
    , smallestFactor
    , recnumFactors
    , mapnumFactors
    , Month(..)
    , Date(..)
    , daysInMonth
    , validDate
    , multiply
    , substitute
    , triads
    , mystery
    ) where

import Data.Maybe
import Data.List

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
  | n > 1 = maxi 1 $ flip (-) 1 $ toInteger $ length $ rmdups $ map (flip nextFactor n) [2..n]
  | n > 0 = 0
  | otherwise = -1

rmdups :: [Integer] -> [Integer]
rmdups = map head . group . sort

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Nov | Dec
  deriving (Eq, Ord, Enum, Show, Read)

daysInMonth :: Month -> Integer -> Integer
daysInMonth m y
  | i == 1 || i == 3 || i == 5 || i == 7 || i == 8 || i == 10 || i == 12 = 31
  | i == 4 || i == 6 || i == 9 || i == 11 = 30
  | mod y 4 == 0  = 29
  | otherwise     = 28
    where i = (+) 1 $ fromJust $ elemIndex m [Jan ..]

data Date = Date Integer Month Integer deriving (Eq, Show)

validDate :: Date -> Bool
validDate (Date d m y)
  | d > 0 && d <= daysInMonth m y = True
  | otherwise = False

multiply :: Num a => [a] -> a
multiply [] = 1
multiply (x:xs) = x * multiply xs

substitute :: Char -> Char -> String -> String
substitute a b = map replace where
  replace x | x == a = b
            | otherwise = x

triads :: Integer -> [(Integer, Integer, Integer)]
triads n = [ (a, b, round c) | a <- [1..n], b <- [1..n]
           , let c = hypotenuse a b
           , a <= b, c <= fromInteger n, isInt c
           ]
  where
    hypotenuse a b = sqrt $ fromInteger $ a ^ 2 + b ^ 2
    isInt x = floor x == ceiling x

mystery xs = foldr (++) [] (map (\y -> [y]) xs)
