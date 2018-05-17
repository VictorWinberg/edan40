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
exec (Comment v : stmts) dict ints = exec stmts dict ints

indent n = (replicate (2*n) ' ')

shw :: Int -> T -> String
shw n (Assignment v e) = indent n ++ v ++ " := " ++ toString e ++ ";" ++ "\n"
shw n (If e s1 s2) = indent n ++ "if " ++ toString e ++ " then\n" ++ shw (n+1) s1 ++ indent n ++ "else\n" ++ shw (n+1) s2
shw n (Begin ss) = indent n ++ "begin\n" ++ concatMap (shw (n+1)) ss ++ indent n ++ "end"
shw n (While e s) = indent n ++ "while " ++ toString e ++ " do\n" ++ shw (n+1) s ++ "\n"
shw n (Read v) = indent n ++ "read " ++ v ++ ";" ++ "\n"
shw n (Write e) = indent n ++ "write " ++ toString e ++ ";" ++ "\n"
shw n (Skip) = indent n ++ "skip" ++ ";" ++ "\n"
shw n (Comment v) = indent n ++ "-- " ++ v ++ "\n"

instance Parse Statement where
  parse = assignment ! ifElse ! begin ! while ! read' ! write ! skip ! comment
  toString = shw 0
