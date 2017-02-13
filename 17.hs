import Data.List.Split (chunksOf)
import Data.Char (digitToInt, isAlpha)

main = print $ total [1..1000]
total :: [Int] -> Int
total = sum . map lengthEnglish
  where lengthEnglish = length . filter isAlpha . inEnglish


ones = (!!) ["" ,"one" ,"two" ,"three" ,"four" ,"five" ,"six" ,"seven" ,"eight" ,"nine",
             "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
             "seventeen", "eighteen", "nineteen"]
tens = (!!) ["" ,"teen" ,"twenty" ,"thirty" ,"forty" ,"fifty" ,"sixty" ,"seventy" ,"eighty" ,"ninety"]
magnitudes = (!!) ["" ," thousand" ," million" ," billion"]

inEnglish :: Int -> String
inEnglish i
  | i == 0 = "zero"
  | i < 20 = ones i
  | i < 100 = format tens 10 "-"
  | i < 1000 = format ((++ " hundred") . ones) 100 " and "
  | otherwise = format ((++ magnitudes m). inEnglish) r " and "
  where
    m = (`div` 3) . lenght . show
    r = 10^(m*3)
    format l mag divider
      | m == 0 = l d
      | otherwise = l d ++ divider ++ inEnglish m
      where (d, m) = i `divMod` mag
