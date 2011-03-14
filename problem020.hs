module Problem020 where

import Data.Char

result = sum $ map digitToInt $ show $ product [1..100]
