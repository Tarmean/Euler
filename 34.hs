import Data.List
import Data.Char

main = print . sum $ filter valid [3..100000]
valid = (==) =<< sum . factDigits
fact = product . enumFromTo 1
factDigits = map (fact . digitToInt) . show
