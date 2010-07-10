-- fibonacci n = round $ (((1 + sqrt 5)/2)^n - ((1 - sqrt 5)/2)^n) / sqrt 5

{-
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)
-}

fibonacci = 1 : 2 : zipWith (+) fibonacci (tail fibonacci)

ans n = sum . takeWhile (< n) . filter even $ fibonacci

main = print $ ans 4000000
