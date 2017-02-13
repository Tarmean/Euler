import Data.List
import Data.Numbers.Primes
triangleNumbers = scanl1 (+) [1..]
factors n = product $ map ((+1) . length) (group (primeFactors n))

main = print $ find ((>500).factors) $ triangleNumbers


