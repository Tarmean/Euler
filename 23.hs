import Data.List
import Data.Numbers.Primes
import Data.Array

main = print $ sum notAbundantSums

notAbundantSums = filter (not . any (abundantArray!) . remainders) [1..high]
  where remainders x = map (x-) $ takeWhile (<= x `div` 2) abundants
abundants = filter (abundantArray !) [1..high]
abundantArray = listArray (1, high) $ map abundant [1..high]
  where abundant = (>) =<< properSum

properSum = (-) =<< product . map (sum . scanl (*) 1) . group . primeFactors
high = 28123
