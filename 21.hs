import Data.List
import Data.Numbers.Primes

-- I am not sure at what point this started to become codegolf light
main = print . sum $ getPair =<< [1..10000]
properSum = (-) =<< product . map (sum . scanl (*) 1) . group . primeFactors
getPair i = let i' = properSum i in if i' > i && properSum i' == i then [i, i'] else []
