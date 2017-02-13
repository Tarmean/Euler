import Data.List
main = print . process . parse =<< readFile "in18.txt"

parse :: String -> [[Int]]
parse = map (map read . words) . lines

-- start from the end and only keep the best paths to reach each slot
process = head . foldr1 sums
  where
    sums cur acc = zipWith combine cur (pairs acc)
    combine p (x, y) = p + max x y
    pairs = zip <*> tail
