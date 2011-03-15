
import Data.List
import Data.Ord

findCycle :: Int -> Int
findCycle x = findCycle' x [1] + 1

findCycle' :: Int -> [Int] -> Int
findCycle' x xs
	| n == 0 = 0
	| elem n xs = 1 + length (takeWhile (/= n) xs)
	| otherwise = findCycle' x (n:xs)
	where
	  n = rem (head xs * 10) x

problem026 :: Int
problem026 = maximumBy (comparing findCycle) [1..1000]

main = print $ problem026
