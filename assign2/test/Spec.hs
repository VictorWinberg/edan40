import StringAlignment

import Test.Tasty
import Test.Tasty.HUnit

similarityScoreTest = testGroup "reduction test"
  [ testCase "haskell pascal" $ similarityScore "HASKELL" "PASCA-L" @?= (-2)
  , testCase "haskell pascal" $ similarityScore "H-ASKELL" "-PASC-AL" @?= (-5)
  ]

allTests = testGroup "all tests"
  [ similarityScoreTest
  ]

main :: IO ()
main = defaultMain allTests
