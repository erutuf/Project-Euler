-- module Problem021 where

import Data.List

d :: Int -> Int
d n = f [1..n] n - n

f :: [Int] -> Int -> Int
f [] _ = 0
f (x:xs) n
	| x*x > n = 0
    | r == 0  = q + x + f xs n
	| otherwise = f xs n
  where
	(q, r) = quotRem n x

findAmicable :: Int -> [(Int, Int)]
findAmicable x = [(a, b) | a <- [2..x-1], b <- [a+1..x], d a == b, d b == a]

main :: IO ()
main = print $ findAmicable 1000
