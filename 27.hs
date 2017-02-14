import Data.Numbers.Primes
import Data.List
import Data.Ord

main = print $ maximumBy (comparing snd) options
options = [(x*y, primeCount x y) | x <- [-999..999], y <- [-1000..1000]]
primeCount a b = length $ takeWhile (isPrime . f) [0..]
  where f n = n^2 + a * n + b
