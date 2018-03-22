{-# OPTIONS_GHC -fwarn-missing-signatures #-}  
main :: IO ()
main = do
  print $ validate 4012888888881881
  print $ validate 4012888888881882
  print $ hanoi 2 "a" "b" "c"
  print $ hanoi 3 "a" "b" "c"

-- double the value of every second digit beginning from the right
-- [1,3,8,6] -> [2,3,16,6]

-- add the digits of the double values

-- calculate the remainder when the sum is divided by 10

-----------------------------------------------------------------
-- CREDIT CARD VALIDATION

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- doubles every other from the left
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) (cycle [1,2])
-- doubleEveryOther = zipWith ($) (cycle [id,(*2)])

-- doubleEveryOther (x:y:ns) = x : y*2 : doubleEveryOther ns
-- doubleEveryOther a = a

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits
-- sumDigits = foldr (+) 0

checksum :: Integer -> Integer
checksum = sumDigits . doubleEveryOther . toDigitsRev

validate :: Integer -> Bool
validate n = (checksum n `mod` 10) == 0

----------------------------------------------------------------
-- TOWERS OF HANOI

type Peg = String
type Move = (Peg, Peg)

-- wow you define the game as an infinite list of moves
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a







