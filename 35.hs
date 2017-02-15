import Data.Array
import Data.Numbers.Primes
import Data.Char

main = print $ length circulars
circulars = filter (circular . toDigits) [1..1000000]
toDigits = map digitToInt . show
circular ls = all isPrime . map toInt $ ls : takeWhile (/=ls) (tail $ iterate turn ls)
  where
    toInt = foldr (\cur acc -> cur + acc * 10) 0
    turn (x:xs) = xs ++ [x]
