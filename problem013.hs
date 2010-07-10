nums = readFile "problem13" >>= return . map read . lines

main = nums >>= putStrLn . take 10 . show . sum
