module Golf where

-- import Safe

skips :: [a] -> [[a]]
skips xs = map ((flip skip) xs) [1..(length xs)]

skip :: Int -> [a] -> [a]
skip n = map snd . filter isIndex . (zip [1..])
    where isIndex = (== 0) . (mod n) . fst

localMaxima :: [Integer] -> [Integer]
localMaxima = map (!! 1) . filter isMaxima . subN

-- needs to consider the 3 next to it
-- convert list into lists of elements [1,2,3,4,5] -> [[1,2,3],[2,3,4],[3,4,5]]
-- then see if one is greater than its neighbords, and return the middle one
-- take 3 xs

-- subN
subN :: [a] -> [[a]]
subN (a:xs) = (a:(take 2 xs)):subN xs
subN _ = []

isMaxima (a:x:b:xs) = (x > a) && (x > b)
isMaxima _ = False




histogram :: [Integer] -> String
histogram ns = map histLine ns ++ "\n==========\n0123456789"

histLine :: Integer -> String
histLine n = ...