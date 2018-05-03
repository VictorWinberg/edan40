import StringAlignment

import Test.Tasty
import Test.Tasty.HUnit

similarityScoreTests = testGroup "reduction tests"
  [ testCase "haskell pascal" $ similarityScore "HASKELL" "PASCAL" @?= (-4)
  , testCase "writers vintner" $ similarityScore "writers" "vintner" @?= (-5)
  ]

attachHeadsTest = testCase "attach heads test" $
  attachHeads 'a' 'b' [("hej","då"), ("k", "bry")] @?= [("ahej","bdå"), ("ak", "bbry")]

maximaByTest = testCase "maximaBy test" $
  maximaBy length ["cs", "efd", "lth", "it"] @?= ["efd", "lth"]

allTests = testGroup "all tests"
  [ similarityScoreTests
  , attachHeadsTest
  , maximaByTest
  ]

main :: IO ()
main = defaultMain allTests
