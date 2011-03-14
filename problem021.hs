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

findAmicable :: Int -> Int
findAmicable x = sum $ [a + b | a <- [2..x-1], let b = d a, d b == a, a < b]

main :: IO ()
main = print $ findAmicable 10000
