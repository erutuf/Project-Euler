-- What is the difference between the sum of the squares and the square of the sums?

sumOfSquares 1 = 1
sumOfSquares x = x^2 + sumOfSquares (x-1)

squareOfSum x = (div ((1+x)*x) 2)^2

main = print $ squareOfSum 100 - sumOfSquares 100
