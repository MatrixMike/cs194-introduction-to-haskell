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

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit n = Mod7 (n `mod` 7)
    add (Mod7 a) (Mod7 b) = lit $ a + b
    mul (Mod7 a) (Mod7 b) = lit $ a * b

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"    
