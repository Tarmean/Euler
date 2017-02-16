import Data.Numbers.Primes
import Data.List

main = print $ firstConsecutive 4
firstConsecutive len = head [idx | (idx, facts) <- zip [1..] $ windows len factors, all ((==len) . length . group) facts]
factors = map primeFactors [1..]
windows i = takeWhile ((==i) . length) . map (take i) . tails
