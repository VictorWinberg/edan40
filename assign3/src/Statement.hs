module Statement(Statement(..), T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
  Assignment String Expr.T |
  If Expr.T Statement Statement |
  Begin [Statement] |
  While Expr.T Statement |
  Read String |
  Write Expr.T |
  Skip |
  Comment String
  deriving (Eq, Show)

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifElse = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIfElse
buildIfElse ((e, s1), s2) = If e s1 s2

begin = accept "begin" -# iter parse #- require "end" >-> Begin

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s

read' = accept "read" -# word #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

comment = accept "--" -# iter (char ? (/= '\n')) #- require "\n" >-> Comment

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment v e : stmts) dict ints = exec stmts (Dictionary.insert (v, Expr.value e dict) dict) ints
exec (If e s1 s2 : stmts) dict ints
  | Expr.value e dict > 0 = exec (s1 : stmts) dict ints
  | otherwise             = exec (s2 : stmts) dict ints
exec (Begin ss : stmts) dict ints = exec (ss ++ stmts) dict ints
exec (While e s : stmts) dict ints
  | Expr.value e dict > 0 = exec (s : While e s : stmts) dict ints
  | otherwise             = exec stmts dict ints
exec (Read v : stmts) dict (int : ints) = exec stmts (Dictionary.insert(v, int) dict) ints
exec (Write e : stmts) dict ints = Expr.value e dict : exec stmts dict ints
exec (Skip : stmts) dict ints = exec stmts dict ints

shw :: T -> String
shw (Assignment v e) = v ++ " := " ++ toString e ++ ";" ++ "\n"
shw (If e s1 s2) = "if " ++ toString e ++ " then " ++ shw s1 ++ " else " ++ shw s2 ++ "\n"
shw (Begin ss) = "begin" ++ concatMap shw ss ++ "end" ++ "\n"
shw (While e s) = "while" ++ toString e ++ "do" ++ shw s ++ "\n"
shw (Read v) = "read " ++ v ++ ";" ++ "\n"
shw (Write e) = "write " ++ toString e ++ ";" ++ "\n"
shw (Skip) = "skip" ++ ";" ++ "\n"
shw (Comment v) = "-- " ++ v ++ "\n"

instance Parse Statement where
  parse = assignment ! ifElse ! begin ! while ! read' ! write ! skip ! comment
  toString = shw
