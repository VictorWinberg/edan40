import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Ex1

maxiTests = testGroup "maxi tests"
  [ testCase "same value" $ maxi 0 0 @?= 0
  , testCase "first value" $ maxi 1 0 @?= 1
  , testCase "second value" $ maxi 0 1 @?= 1
  ]

recsumsqTests = testGroup "recsumsq tests"
  [ testCase "zero" $ recsumsq 0 @?= 0
  , testCase "one" $ recsumsq 1 @?= 1
  , testCase "two" $ recsumsq 2 @?= 5
  , testCase "three" $ recsumsq 3 @?= 14
  , testCase "negative" $ recsumsq (-1) @?= -1
  ]

mapsumsqTests = testGroup "mapsumsq tests"
  [ testCase "zero" $ mapsumsq 0 @?= 0
  , testCase "one" $ mapsumsq 1 @?= 1
  , testCase "two" $ mapsumsq 2 @?= 5
  , testCase "three" $ mapsumsq 3 @?= 14
  , testCase "negative" $ recsumsq (-1) @?= -1
  ]

rechanoiTests = testGroup "rechanoi tests"
  [ testCase "one" $ rechanoi 1 @?= 1
  , testCase "two" $ rechanoi 2 @?= 3
  , testCase "three" $ rechanoi 3 @?= 7
  , testCase "four" $ rechanoi 4 @?= 15
  , testCase "five" $ rechanoi 5 @?= 31
  , testCase "negative" $ rechanoi (-1) @?= -1
  ]

maphanoiTests = testGroup "maphanoi tests"
  [ testCase "one" $ maphanoi 1 @?= 1
  , testCase "two" $ maphanoi 2 @?= 3
  , testCase "three" $ maphanoi 3 @?= 7
  , testCase "four" $ maphanoi 4 @?= 15
  , testCase "five" $ maphanoi 5 @?= 31
  , testCase "negative" $ maphanoi (-1) @?= -1
  ]

smallestFactorTests = testGroup "smallestFactor tests"
  [ testCase "1" $ smallestFactor 1 @?= 1
  , testCase "14" $ smallestFactor 14 @?= 2
  , testCase "15" $ smallestFactor 15 @?= 3
  , testCase "negative" $ smallestFactor (-1) @?= -1
  ]

recnumFactorsTests = testGroup "recnumFactors tests"
  [ testCase "1" $ recnumFactors 1 @?= 0
  , testCase "2" $ recnumFactors 2 @?= 1
  , testCase "3" $ recnumFactors 3 @?= 1
  , testCase "4" $ recnumFactors 4 @?= 2
  , testCase "14" $ recnumFactors 14 @?= 2
  , testCase "15" $ recnumFactors 15 @?= 2
  , testCase "negative" $ recnumFactors (-1) @?= -1
  ]

mapnumFactorsTests = testGroup "mapnumFactors tests"
  [ testCase "1" $ mapnumFactors 1 @?= 0
  , testCase "2" $ mapnumFactors 2 @?= 1
  , testCase "3" $ mapnumFactors 3 @?= 1
  -- , testCase "4" $ mapnumFactors 4 @?= 2
  , testCase "14" $ mapnumFactors 14 @?= 2
  , testCase "15" $ mapnumFactors 15 @?= 2
  , testCase "negative" $ mapnumFactors (-1) @?= -1
  ]

multiplyTests = testGroup "multiply tests"
  [ testCase "empty list" $ multiply [] @?= 1
  , testCase "single element" $ multiply [1] @?= 1
  , testCase "two elements" $ multiply [2,3] @?= 6
  , testCase "multiple elements" $ multiply [1,2,3,4,5] @?= 120
  ]

substituteTests = testGroup "substitute tests"
  [ testCase "unchanged" $ substitute 'j' 's' "haskell" @?= "haskell"
  , testCase "one letter" $ substitute 'v' 's' "victor" @?= "sictor"
  , testCase "two letters" $ substitute 'e' 'i' "eigenvalue" @?= "iiginvalui"
  ]

triadsTests = testGroup "triads tests"
  [ testCase "empty" $ triads 4 @?= []
  , testCase "3 4 5" $ triads 5 @?= [(3,4,5)]
  ]

propertyTests = testGroup "property tests"
  [ testProperty "recsumsq same as mapsumsq" sumsqRecSameAsMap
  , testProperty "rechanoi same as maphanoi" hanoiRecSameAsMap
  -- , testProperty "recnumFactors same as mapnumFactors" numFactorsRecSameAsMap
  , testProperty "multiply same as product" multiplySameAsProduct
  ]

sumsqRecSameAsMap :: Integer -> Bool
sumsqRecSameAsMap x = recsumsq x == mapsumsq x

hanoiRecSameAsMap :: Integer -> Bool
hanoiRecSameAsMap x = rechanoi x == maphanoi x

numFactorsRecSameAsMap :: Integer -> Bool
numFactorsRecSameAsMap x = recnumFactors x == mapnumFactors x

multiplySameAsProduct :: [Integer] -> Bool
multiplySameAsProduct list = multiply list == product list

allTests = testGroup "all tests"
  [ maxiTests
  , recsumsqTests
  , mapsumsqTests
  , rechanoiTests
  , maphanoiTests
  , smallestFactorTests
  , recnumFactorsTests
  , mapnumFactorsTests
  , multiplyTests
  , substituteTests
  , triadsTests
  , propertyTests
  ]

main :: IO ()
main = defaultMain allTests
