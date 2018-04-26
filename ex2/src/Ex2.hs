module Ex2
    ( Proposition(..)
    , vars
    , truthValue
    , deep
    ) where

import Data.Maybe
import Data.List

data Proposition = VAR String
  | AND Proposition Proposition
  | OR Proposition Proposition
  | NOT Proposition
  deriving (Eq, Show)

vars :: Proposition -> [String]
vars (VAR p) = [p]
vars (NOT p) = vars p
vars (AND p q) = vars p ++ vars q
vars (OR p q) = vars p ++ vars q

truthValue :: Proposition -> [(String, Bool)] -> Bool
truthValue (VAR p) = fromJust . lookup p
truthValue (NOT p) = not . truthValue p
truthValue (AND p q) = deep (&&) (truthValue p) (truthValue q)
truthValue (OR p q) = deep (||) (truthValue p) (truthValue q)

deep :: (Bool -> Bool -> Bool) -> (a -> Bool) -> (a -> Bool) -> a -> Bool
deep op a b pq = op (a pq) (b pq)
