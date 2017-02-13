main = print $ 40 `choose` 20
factorial = product . enumFromTo 1
choose n k = factorial n `div` (factorial k * factorial (n-k))
