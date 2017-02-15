import qualified Data.IntSet as S

pentagonal = map p [1..2500]
  where p n = n * (3*n-1) `div` 2
isPentagonal = (`S.member` pset)
pset = S.fromList pentagonal

answers = [a-b| a <- pentagonal, b <- upTo a pentagonal, isPentagonal $ a-b, isPentagonal $ a+b]
upTo = takeWhile . (>)

main = print $ head answers
