module Problem019 where

import Data.List

bigmonth = [1, 3, 5, 7, 8, 10, 12]

(+%) :: Int -> Int -> Int
a +% b = rem (a + b) 7

inc :: (Int, Int, Int) -> Int
inc (year, month, days)
	| month == 2 = chkleap year +% days
	| elem month bigmonth = 3 +% days
	| otherwise = 2 +% days
	where
	  chkleap y
	  	| rem y 400 == 0 = 1
		| rem y 100 == 0 = 0
		| rem y 4 == 0  = 1
		| otherwise = 0

begin = (1901, 1, 3)

stepYM :: (Int, Int, Int) -> (Int, Int, Int)
stepYM t@(y, 12, d) = (y+1, 1, inc t)
stepYM t@(y, m, d)  = (y, m+1, inc t)

fst3 (a, _, _) = a
thd3 (_, _, c) = c

days = unfoldr (\x -> if fst3 x == 2001 then Nothing else Just (thd3 x, stepYM x)) begin 

result = length $ filter ( == 0) days
