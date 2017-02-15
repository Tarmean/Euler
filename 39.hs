import Data.List
import Data.Ord
main = print $ maximumBy (comparing $ length . solutions) [1..1000]
solutions total = [(x, y, z) | x <- [1..total], y <- [x..total], let z = total - x - y, z^2 == x^2 + y^2]
