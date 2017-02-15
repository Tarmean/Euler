import Data.List

main = print $ sum . map toNum $ pandigitals
pandigitals = filter valid $ permutations [0..9]

valid = and . zipWith divisible primes . slices 3
toNum = foldl' ((+) . (*10)) 0
primes = [2, 3, 5, 7, 11, 13, 17]
slices i = map toNum . takeWhile ((==i) . length) . map (take i) . tails . tail
divisible i = (==0) . (`mod` i)


