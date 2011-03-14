module Problem017 where

zeroToNineteen = [0, 3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 9, 8, 7, 7, 9, 8, 8]
tenMult = [6, 6, 5, 5, 5, 7, 6, 6]
hundred = 7

letternum :: Int -> Int
letternum n
	| n == 0   = 0
	| n < 20   = zeroToNineteen !! n
	| n < 100  = tenMult !! (quot n 10 - 2) + (zeroToNineteen !! (rem n 10))
	| n < 1000 = zeroToNineteen !! (quot n 100) + hundred + (\x -> if x == 0 then x else x + 3) (letternum $ rem n 100)
	| otherwise= 11

result = foldr (\x y -> letternum x + y) 0 [1..1000]
