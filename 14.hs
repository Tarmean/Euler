import Data.Array
import Data.List (maximumBy)
import Data.Ord (comparing)

series _ 1 = 0
series f n = 1 + f (step n)
step i
  | even i = i `div` 2
  | otherwise = 3 * i + 1

fixArray max f = array
  where 
    base = f memoized
    memoized i
      | i <= max = array ! i
      | otherwise = base i
    array = listArray (1, max) $ map base [1..max]

memF = fixArray 1000000 series
main = print $ maximumBy (comparing snd) $ assocs memF
