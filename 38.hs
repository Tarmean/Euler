import Data.List

main = print $ find valid $ map panDigital [9837, 9836..]
panDigital = concat . map show . ($[1, 2]) . map . (*)
valid = (== ['1'..'9']) . sort
