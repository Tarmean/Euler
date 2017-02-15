import Data.Numbers.Primes
import Data.Char
import Data.List

toDigits = map digitToInt . show
fromDigits = foldl' (\acc cur -> acc * 10 + cur) 0

allPrime i = slicesPrime (init . tails $ digits) && slicesPrime (tail . inits $ digits)
  where digits = toDigits i
slicesPrime = all (isPrime . fromDigits)

main = print $ sum . take 11 $ filter allPrime [8..]
