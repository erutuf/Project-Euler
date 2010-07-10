{-
isPalindrome x = show x == (reverse $ show x)

main = print $ maximum [m*n | m <- [100..999], n <- [m..999], isPalindrome (m*n)]
-}
palindromes = [100001*a + 10010*b + 1100*c | a <- [9,8..1], b <- [9,8..0], c <- [9,8..0]]

interval x a b = x >= a && x <= b

main = print $ head $ filter helper palindromes
	where
		helper x = helper' x [100..999]
		helper' _ [] = False
		helper' x (n:rest)
			| mod x n == 0 && interval (div x n) 100 999 = True
			| otherwise = helper' x rest

