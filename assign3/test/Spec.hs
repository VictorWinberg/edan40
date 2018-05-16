import Prelude hiding (return, fail)
import Parser
import qualified Dictionary
import Expr

import Test.Tasty
import Test.Tasty.HUnit

letterTest = testGroup "parser test"
    [ testCase "abc" $ letter "abc" @?= Just('a',"bc")
    , testCase "digits " $ letter "123" @?= Nothing
    , testCase "empty " $ letter "" @?= Nothing
    ]

spacesTest = testGroup "spaces test"
    [ testCase "no spaces" $ spaces "abc" @?= Just("","abc")
    , testCase "space and tab" $ spaces "  \t abc" @?= Just("  \t ","abc")
    ]

charsTest = testGroup "chars test"
    [ testCase "2 chars" $ chars 2 "abc" @?= Just ("ab","c")
    , testCase "0 chars" $ chars 0 "ab" @?= Just ("","ab")
    , testCase "3 chars" $ chars 3 "ab" @?= Nothing
    ]

requireTest = testGroup "require test"
    [ testCase "require ok" $ require ":=" ":= 1" @?= Just (":=","1")
    , testCase "require error" $ require "else" "then" @?= Nothing -- Program error: expecting else near then
    ]

acceptTest = testCase "accept test" $ (accept "read" -# word) "read count" @?= Just ("count","")

parserTests = testGroup "all tests"
    [ letterTest
    , spacesTest
    , charsTest
    , requireTest
    , acceptTest
    ]

x = 1
y = 2
dict = Dictionary.insert ("x", 1) $
       Dictionary.insert ("y", 2) $
       Dictionary.empty

testValue string = value (fromString string) dict
exprTest = testGroup "exprTest"
    [ testCase "1 integer" $ testValue "1"  @?= 1
    , testCase "x" $ testValue "x"  @?= x
    , testCase "x+y" $ testValue "x+y"  @?= x + y
    , testCase "x-y-y" $ testValue "x-y-y"  @?= x - y -y
    ]
allTests = testGroup "all tests"
    [ parserTests
    , exprTest
    ]

main :: IO ()
main = defaultMain allTests
