import Control.Monad
import Data.List
main = putStrLn $  lastN 10 . show . sum $ take 1000 series
series = map (join (^)) [1..]
lastN n xs = foldl' (const . drop 1) xs (drop n xs)
