module Calc where

import ExprT
import Parser
import Control.Applicative

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s

