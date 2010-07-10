import Data.List

base max = 2:[3, 5..floor . sqrt . fromInteger $ max]

findMaxPF :: Integer -> Integer
{-
findMaxPF x = case (find (\n -> mod x n == 0) (base x)) of
		Just m  -> findMaxPF $ div x m
		Nothing -> x
-}
findMaxPF x = maybe x (findMaxPF . div x) (find (\n -> mod x n == 0) (base x))

main = print $ findMaxPF 600851475143
