-- main = print $ sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]

ans x = div (ap 3 + ap 5 - ap 15) 2
	where
		numOfTerms y = div (x-1) y
		ap y         = numOfTerms y * (y * 2 + (numOfTerms y - 1) * y)

main = print $ ans 1000
