import Data.List

main :: IO ()
main = undefined

-- double the value of every second digit beginning from the right
-- [1,3,8,6] -> [2,3,16,6]

-- add the digits of the double values

-- calculate the remainder when the sum is divided by 10

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
checksum n = sumDigits (doubleEveryOther (toDigitsRev n))

validate :: Integer -> Bool
validate n = (checksum n `mod` 10) == 0