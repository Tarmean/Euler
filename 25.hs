import Data.List
fibs = 1:1:zipWith (+) fibs (tail fibs)
main = print $ succ <$> findIndex (>10^999) fibs
