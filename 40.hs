import Data.Char

main = print . product $ digit <$> indices
digit = digitToInt . (!!) (show =<< [1..]) . pred
indices = take 7 $ iterate (*10) 1
