import Data.List

select :: [a] -> [(a, [a])]
select = map merge . splits
  where merge (ls, r:rs) = (r, ls++rs)
        splits = init . (zip <$> inits <*> tails)

masks :: Int -> [[Bool]]
masks i = go (replicate (i-3) True) [False, False, False] []
  where
    go :: [Bool] ->[Bool] -> [Bool] -> [[Bool]]
    go [] [] ls = return ls
    go t [] ls = return (t++ls)
    go [] f ls = return (f++ls)
    go (x:xs) (y:ys) ls = go xs (y:ys) (x:ls) ++ go (x:xs) ys (y:ls)

family :: Int -> [Int] -> [[Int]]
family i ls = map merge $ masks (length ls)
  where
   merge mask = zipWith go mask ls
   go True _ = i
   go False j = j
  

