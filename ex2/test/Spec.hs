import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Ex2

pq = [("p", True), ("q", False)]

propositionTests = testGroup "proposition tests"
  [ testCase "p" $ truthValue (VAR "p") pq @?= True
  , testCase "!p" $ truthValue (NOT (VAR "p")) pq @?= False
  , testCase "p && q" $ truthValue (AND (VAR "p") (VAR "q")) pq @?= False
  , testCase "p && !q" $ truthValue (AND (VAR "p") (NOT (VAR "q"))) pq @?= True
  , testCase "p || q" $ truthValue (OR (VAR "p") (VAR "q")) pq @?= True
  ]

allTests = testGroup "all tests"
  [ propositionTests
  ]

main :: IO ()
main = defaultMain allTests
