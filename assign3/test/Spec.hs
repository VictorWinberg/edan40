import Prelude hiding (return, fail)
import Parser
import qualified Dictionary
import Expr
import Statement
import Program
import TestProgram

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
  -- , testCase "require error" $ require "else" "then" @?= Exception: expecting else near then
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
  -- , testCase "1/(2-y)" $ testValue "1/(2-y)" @?= Exception: division by 0
  -- , testCase "2+z" $ testValue "2+z" @?= Exception: undefined variable z
  ]

whileString = "while n do begin fac:=fac*n; n:=n-1; end"

statementTest = testGroup "statement test"
  [ testCase "skip" $ fromString "skip;"
      @?= Skip
  , testCase "read" $ fromString "read count;"
      @?= Read "count"
  , testCase "write" $ fromString "write count+1;"
      @?= Write (Add (Var "count") (Num 1))
  , testCase "count" $ fromString "count := 0;"
      @?= Assignment "count" (Num 0)
  , testCase "begin" $ fromString "begin skip; end"
      @?= Begin [Skip]
  , testCase "begin" $ fromString "begin x:=0; x:=x+1; end"
      @?= Begin [Assignment "x" (Num 0), Assignment "x" (Add (Var "x") (Num 1))]
  , testCase "if" $ fromString "if x then skip; else x:=0-x;"
      @?= If (Var "x") Skip (Assignment "x" (Sub (Num 0) (Var "x")))
  , testCase "while" $ fromString "while n do n:=n-1;"
      @?= While (Var "n") (Assignment "n" (Sub (Var "n") (Num 1)))
  , testCase "while" $ fromString whileString
      @?= While (Var "n") (Begin [Assignment "fac" (Mul (Var "fac") (Var "n")), Assignment "n" (Sub (Var "n") (Num 1))])
  , testCase "begin" $ fromString  "begin read x ; x := x + 1 ; write x; end"
      @?= Begin [Read "x", Assignment "x" (Add (Var "x") (Num 1)), Write (Var "x")]
  , testCase "begin" $ fromString  ("begin read n; fac:=1; " ++ whileString ++ " write fac; end")
      @?= Begin [Read "n", Assignment "fac" (Num 1), While (Var "n") (Begin [Assignment "fac" (Mul (Var "fac") (Var "n")), Assignment "n" (Sub (Var "n") (Num 1))]), Write (Var "fac")]
  ]

p' = "\
\read k;\n\
\read n;\n\
\m := 1;\n\
\while n-m do\n\
\  begin\n\
\    if m-m/k*k then\n\
\      skip;\n\
\    else\n\
\      write m;\n\
\    m := m+1;\n\
\  end\n"

p1' = "\
\read n;\n\
\read b;\n\
\m := 1;\n\
\s := 0;\n\
\p := 1;\n\
\while n do\n\
\  begin\n\
\    q := n/b;\n\
\    r := n-q*b;\n\
\    write r;\n\
\    s := p*r+s;\n\
\    p := p*10;\n\
\    n := q;\n\
\  end\n\
\write s;\n"

p4' = "\
\read a;\n\
\read b;\n\
\-- a comment\n\
\s := 3;\n\
\while a do\n\
\  begin\n\
\    c := a^s;\n\
\    d := 2^a;\n\
\    write c;\n\
\    write d;\n\
\    a := a-1;\n\
\  end\n\
\write a;\n"

programTest = testGroup "program test"
  [ testCase "p" $ toString p @?= p'
  , testCase "p1" $ toString p1 @?= p1'
  , testCase "p2" $ toString p2 @?= p'
  , testCase "p3" $ toString p3 @?= p1'
  , testCase "p4" $ toString p4 @?= p4'
  , testCase "rp" $ Program.exec p [3,16] @?= [3, 6, 9, 12, 15]
  , testCase "rp1" $ Program.exec p1 [1024, 2] @?= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 10000000000]
  , testCase "rp4" $ Program.exec p4 [4,4] @?= [64, 16, 27, 8, 8, 4, 1, 2, 0]
  ]

allTests = testGroup "all tests"
  [ parserTests
  , exprTest
  , statementTest
  , programTest
  ]

main :: IO ()
main = defaultMain allTests
