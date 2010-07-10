
sieve (x:rest) = x : sieve (filter (\y -> mod y x /= 0) rest)

primes = sieve [2..]
ans x = primes !! (x-1)
{-
-- isPrime a = isPrime' a primes

isPrime' (p:ps) a
	| p*p > a = True
	| mod a p == 0 = False
	| otherwise = isPrime' ps a

primes = 2 : filter (isPrime' primes) [3..]
-}
main = print $ ans 10001
