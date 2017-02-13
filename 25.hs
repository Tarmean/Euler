import Data.List
fibs = 1:1:zipWith (+) fibs (tail fibs)
main = print $ succ <$> findIndex ((>=1000).digits) fibs

digits = length . show
