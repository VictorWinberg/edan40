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
  Skip
  deriving (Eq, Show)

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifElse = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIfElse
buildIfElse ((e, s1), s2) = If e s1 s2

begin = accept "begin" -# iter parse #- require "end" >-> Begin

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite (e) = Write e

read2 = accept "read" -# word #- require ";" >-> buildRead
buildRead (s) = Read s

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip (_) = Skip

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input


instance Parse Statement where
  parse = assignment ! ifElse ! while ! write ! read2 ! skip
  toString = error "Statement.toString not implemented"
