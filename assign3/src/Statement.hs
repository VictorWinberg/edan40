module Statement(T, parse, toString, fromString, exec) where
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
  Skip
  deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifElse = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIfElse
buildIfElse ((e, s1), s2) = If e s1 s2

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input


instance Parse Statement where
  parse = assignment ! ifElse ! while
  toString = error "Statement.toString not implemented"
