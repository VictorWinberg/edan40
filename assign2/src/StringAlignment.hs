module StringAlignment
    ( AlignmentType(..)
    , similarityScore
    , attachHeads
    , maximaBy
    , optAlignments
    , optAlignments'
    , outputOptAlignments
    , mcsLength
    , mcsLength'
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

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs ++ [h1],ys ++ [h2]) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = []
maximaBy f xs = filter (\x -> f x == maxi) xs
  where maxi = maximum $ map f xs

type AlignmentType = (String,String)

-- which returns a list of all optimal alignments between string1 and string2.
optAlignments' :: String -> String -> [AlignmentType]
optAlignments' xs ys = maximaBy alignScore $ alignments xs ys
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

optAlignments :: String -> String -> [AlignmentType]
optAlignments xs ys = maximaBy alignScore $ alignmentsLen (length xs) (length ys)
  where 
    alignmentsLen i j = alignmentsTable!!i!!j
    alignmentsTable = [[ alignmentsEntry i j | j <- [0..]] | i<-[0..] ]

    alignmentsEntry :: Int -> Int -> [AlignmentType]
    alignmentsEntry 0 0 = [("", "")]
    alignmentsEntry i 0 = attachTails (get_x i) '-' $ alignmentsLen (i-1) 0
    alignmentsEntry 0 j = attachTails '-' (get_y j) $ alignmentsLen 0 (j-1)
    alignmentsEntry i j = maximaBy alignScore $ concat
      [ attachTails (get_x i) (get_y j) $ alignmentsLen (i-1) (j-1)
      , attachTails (get_x i) '-' $ alignmentsLen (i-1) j
      , attachTails '-' (get_y j) $ alignmentsLen i (j-1)
      ]
    alignScore ([], []) = 0
    alignScore ((x:xs),(y:ys)) = score x y + alignScore (xs,ys)
    get_x i = xs!!(i-1)
    get_y j = ys!!(j-1)

mcsLength' :: Eq a => [a] -> [a] -> Int
mcsLength' _ [] = 0
mcsLength' [] _ = 0
mcsLength' (x:xs) (y:ys)
 | x == y    = 1 + mcsLength' xs ys
 | otherwise = max (mcsLength' xs (y:ys))
                   (mcsLength' (x:xs) ys)

mcsLength :: Eq a => [a] -> [a] -> Int
mcsLength xs ys = mcsLen (length xs) (length ys)
 where
   mcsLen i j = mcsTable!!i!!j
   mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]

   mcsEntry :: Int -> Int -> Int
   mcsEntry _ 0 = 0
   mcsEntry 0 _ = 0
   mcsEntry i j
     | x == y    = 1 + mcsLen (i-1) (j-1)
     | otherwise = max (mcsLen i (j-1))
                       (mcsLen (i-1) j)
     where
        x = xs!!(i-1)
        y = ys!!(j-1)

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
  let opt = optAlignments string1 string2
  putStrLn $ "There are " ++ show (length(opt)) ++ " optimal alignments: \n"
  mapM (\(a,b) -> putStrLn (a ++ "\n" ++ b ++ "\n")) $ opt
  putStrLn $ "There were " ++ show (length(opt)) ++ " optimal alignments!"
