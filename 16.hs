import Data.Char
sumDigits = sum . map digitToInt . show
main = print . sumDigits $ 2^1000
