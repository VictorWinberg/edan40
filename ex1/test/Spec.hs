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
  ]

mapsumsqTests = testGroup "mapsumsq tests"
  [ testCase "zero" $ mapsumsq 0 @?= 0
  , testCase "one" $ mapsumsq 1 @?= 1
  , testCase "two" $ mapsumsq 2 @?= 5
  , testCase "three" $ mapsumsq 3 @?= 14
  ]

allTests = testGroup "all tests"
  [ maxiTests
  , recsumsqTests
  , mapsumsqTests
  ]

main :: IO ()
main = defaultMain allTests
