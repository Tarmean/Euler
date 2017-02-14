import Data.Char
main = print . pred . sum $ filter valid [1..200000]
valid = (==) =<< powerSum
powerSum = sum . map (^5) . map digitToInt . show
