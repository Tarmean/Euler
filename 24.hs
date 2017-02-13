import Data.Char
import Data.List

main = putStrLn . map intToDigit $ (sort $ permutations [0..9]) !! 999999
