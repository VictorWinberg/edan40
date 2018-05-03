module StringAlignment
    ( AlignmentType(..)
    , similarityScore
    , attachHeads
    , maximaBy
    , optAlignments
    , outputOptAlignments
    ) where

scoreMatch = 0
scoreMismatch = (-1)
scoreSpace = (-1)

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] _ = scoreSpace
similarityScore _ [] = scoreSpace
similarityScore (x:xs) (y:ys) = maximum
  [ similarityScore xs ys + score x y
  , similarityScore xs (y:ys) + score x '-'
  , similarityScore (x:xs) ys + score '-' y
  ]

score :: Char -> Char -> Int
score x '-'   = scoreSpace
score '-' y   = scoreSpace
score x y
  | x == y    = scoreMatch
  | otherwise = scoreMismatch

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

type AlignmentType = (String,String)

-- which returns a list of all optimal alignments between string1 and string2.
optAlignments :: String -> String -> [AlignmentType]
optAlignments xs ys = maximaBy alignScore $ alignments xs ys
  where alignments [] [] = [("", "")]
        alignments (x:xs) [] = attachHeads x '-' $ alignments xs []
        alignments [] (y:ys) = attachHeads '-' y $ alignments [] ys
        alignments (x:xs) (y:ys) = concat
          [ attachHeads x y $ alignments xs ys
          , attachHeads x '-' $ alignments xs (y:ys)
          , attachHeads '-' y $ alignments (x:xs) ys
          ]
        alignScore ([], []) = 0
        alignScore ((x:xs),(y:ys)) = score x y + alignScore (xs,ys)


outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
  putStr "There are "
  putStr (show (length(attachHeads 'a' 'b' [("abc", "cde"), ("efg", "hij")])))
  putStrLn " optimal alignments: \n"

  mapM_ (\(a,b) -> putStrLn (a++"\n"++b++"\n")) $ attachHeads 'a' 'b' [("abc", "def"), ("ghi", "jkl")]


  putStr "There were "
  putStr (show (length(attachHeads 'a' 'b' [("abc", "cde"), ("efg", "hij")])))
  putStr " optimal alignments!\n"
