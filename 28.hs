main = print $ totalSum 1001
totalSum n = sum $ take ringCount $ ringSums
  where ringCount = pred n `div` 2 + 1

ringSums = 1 : step 3 2
step base size = curSum : step base' size'
  where base' = 4*size + base + 2
        size' = size + 2
        curSum = 4*base + 6*size
