module Problem022 where

import Data.Char
import Data.List

chToNum :: Char -> Int
chToNum c
	| c <= 'Z' && 'A' <= c = ord c - ord 'A' + 1
	| otherwise            = 0

score :: String -> Int -> Int
score name line = ( sum $ map chToNum name ) * line

total :: [String] -> Int
total cs = sum $ zipWith score cs [1..]

result str = total $ sort $ read $ "[" ++ str ++ "]"
