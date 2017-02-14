main = print $ coins 200 sizes
sizes = [200, 100, 50, 20, 10, 5, 2, 1]
coins 0 _ = 1
coins _ [] = 0
coins rest (x:xs)
  | rest >= x = coins (rest - x) (x:xs) + coins rest xs 
  | otherwise = coins rest xs 
