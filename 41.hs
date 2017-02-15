import Data.List (permutations, inits, foldl')
import Data.Numbers.Primes (isPrime)

main = print $ maximum possibilities
possibilities = filter isPrime . map toNum $ permutations =<< inits [1..9]
toNum = foldl' (\acc cur -> acc * 10 + cur) 0
