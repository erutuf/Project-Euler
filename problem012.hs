-- module Problem012 where

divisor :: Int -> Int
divisor n = length $ divisor' n [1..n]

divisor' n (x:xs)
	| n < x*x      = []
	| rem n x == 0 = x : quot n x : divisor' n xs
	| otherwise    = divisor' n xs

ans :: Int
ans = head [n | n <- map (\m -> quot ((1+m)*m) 2) [501..], divisor n >= 501]

main = print ans
