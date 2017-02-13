import Data.Char
main = print $ factSum 100
sumDigits = sum . map digitToInt . show
factSum = sumDigits . product . enumFromTo 1
