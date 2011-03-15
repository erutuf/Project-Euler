
import Data.Array
import Euler

lim = 28123

isAbundant :: Int -> Bool
isAbundant x = x < sumOfDivs x

abundantArray :: Array Int Bool
abundantArray = listArray (1,lim) $ map isAbundant [1..lim]

abundantList :: [Int]
abundantList = filter (abundantArray !) [1..lim]

problem23 :: Int
problem23 = sum $ filter isnotSum [1..lim]
	where
	  isnotSum x = not $ any  (abundantArray !) [x-a | a <- takeWhile (<= quot x 2) abundantList]

main :: IO ()
main = print problem23
