import StringAlignment

import Test.Tasty
import Test.Tasty.HUnit

similarityScoreTests = testGroup "reduction tests"
  [ testCase "haskell pascal" $ similarityScore "HASKELL" "PASCAL" @?= (-4)
  , testCase "victor rotciv" $ similarityScore "victor" "rotciv" @?= (-6)
  , testCase "writers vintner" $ similarityScore "writers" "vintner" @?= (-5)
  ]

attachHeadsTest = testCase "attach heads test" $
  attachHeads 'a' 'b' [("hej","då"), ("k", "bry")] @?= [("ahej","bdå"), ("ak", "bbry")]

maximaByTest = testCase "maximaBy test" $
  maximaBy length ["cs", "efd", "lth", "it"] @?= ["efd", "lth"]

optimalAlignPrimeTest = testCase "optimal align prime" $
  optAlignments' "writers" "vintner" @?= [("writ-ers","vintner-"), ("wri-t-ers","v-intner-"), ("wri-t-ers","-vintner-")]

optimalAlignTest = testGroup "optimal align tests"
  [ testCase "easy" $ optAlignments "writers" "vintner" @?= [("writ-ers","vintner-"), ("wri-t-ers","-vintner-"), ("wri-t-ers","v-intner-")]
  -- , testCase "medium " $ optAlignments "aferociousmonadatemyhamster" "functionalprogrammingrules" @?= [("writ-ers","vintner-"), ("wri-t-ers","v-intner-"), ("wri-t-ers","-vintner-")]
  ]

allTests = testGroup "all tests"
  [ similarityScoreTests
  , attachHeadsTest
  , maximaByTest
  , optimalAlignPrimeTest
  , optimalAlignTest
  ]

main :: IO ()
main = defaultMain allTests
