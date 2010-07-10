-- What is the smallest number that is evenly divisible by all of the numbers form 1 to 20?

ans []          = 1
ans (x:[])      = x
ans (x:x':rest) = ans (lcm x x' : rest)

main = print $ ans [1..20]
