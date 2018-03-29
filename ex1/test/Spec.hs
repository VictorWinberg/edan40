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

propertyTests = testGroup "property tests"
  [ testProperty "recsumsq same as mapsumsq" sumsqRecSameAsMap
  , testProperty "rechanoi same as maphanoi" hanoiRecSameAsMap
  ]

sumsqRecSameAsMap :: Integer -> Bool
sumsqRecSameAsMap x = recsumsq x == mapsumsq x

hanoiRecSameAsMap :: Integer -> Bool
hanoiRecSameAsMap x = rechanoi x == maphanoi x

allTests = testGroup "all tests"
  [ maxiTests
  , recsumsqTests
  , mapsumsqTests
  , rechanoiTests
  , maphanoiTests
  , propertyTests
  ]

main :: IO ()
main = defaultMain allTests
