primesInt :: [Int]
primesInt = 2 : primes'
	where
		primes' = 3 : sieve 0 5
		sieve i x = filter isPrime [x, x+2..p*p-2] ++ sieve (i+1) (p*p+2)
	 	  where
			(ps, p:_) = splitAt i primes'
			isPrime x = all ((/=0) . rem x) ps

primes :: [Integer]
primes = map fromIntegral primesInt

ans x = sum . takeWhile (< x) $ primes

main = print $ ans 2000000
