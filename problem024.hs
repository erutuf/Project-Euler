
import Data.List

problem024 :: Int -> [Int]
problem024 n = problem024' (n-1) 9 [0..9]

problem024' :: Int -> Int -> [Int] -> [Int]
problem024' m a xs
	| r == 0 = i:delete i xs
	| otherwise = i:(problem024' r (a-1) (delete i xs))
	where
	  (x, r) = quotRem m $ product [1..a]
	  i = xs !! x

main = print $ problem024 1000000
