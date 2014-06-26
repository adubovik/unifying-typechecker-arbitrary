module Exp where

data Exp = Add Exp Exp
         | Lit Int
         | Eq Exp Exp
  deriving Show

