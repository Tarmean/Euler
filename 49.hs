import Data.Function
import Data.List
import Data.Numbers.Primes

-- At first I was very confused by this task because I thought all numbers had to be selected from 4 `choose` [1..9]
-- In which case 148748178147 is the only solution for four digits
main = mapM_ putStrLn increasing

isPerm = (==) `on` sort . show
primesT4 = filter isPrime [1000..9999]
increasing = [ show =<< [j, i, h]
             | h <- primesT4
             , i <- takeWhile (<h) primesT4
             , isPerm h i
             , j <- takeWhile (<i) primesT4
             , isPerm h j
             , i - j == h - i]
