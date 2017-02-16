import Data.Numbers.Primes

main = print . head $ filter nonePrime composites

notPrime = not . isPrime
composites = filter notPrime [3, 5..]
nonePrime o = all notPrime candidates
  where
    squares = takeWhile (< o) $ (^2) <$> [1..]
    candidates = (o-).(2*) <$> squares
