module Fibonacci where

import Data.List


-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fibsl :: [Integer]
fibsl = map fib [0..]


-- Exercise 2
fibs2 :: [Integer]
fibs2 = fibsScan

-- from the tubes
fibsZip :: [Integer]
fibsZip = 0 : 1 : zipWith (+) fibsZip (tail fibsZip)

-- [1]
-- [1, 2]
-- [1, 2, 4]

-- [0, 1]
-- [0, 1, 1]
-- [0, 1, 1, 2]
fibsScan :: [Integer]
fibsScan = 0 : scanl (+) 1 fibsScan
