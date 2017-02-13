import Data.List.Split (chunksOf)

main = print $ total [1..1000]
total :: [Int] -> Int
total = sum . map lengthEnglish
  where lengthEnglish = length . filter isAlpha . transform


ones = (!!) ["" ,"one" ,"two" ,"three" ,"four" ,"five" ,"six" ,"seven" ,"eight" ,"nine",
             "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
             "seventeen", "eighteen", "nineteen"]
tens = (!!) ["" ,"teen" ,"twenty" ,"thirty" ,"forty" ,"fifty" ,"sixty" ,"seventy" ,"eighty" ,"ninety"]
magnitudes = (!!) [" hundred", " thousand" ," million" ," billion"]

transform i
  | i < 0 = "minus " ++ transform (-i)
  | i == 0 = "zero"
  | i < 20 = ones i
  | i < 100 = format tens 10 "-" i
  | otherwise = withMag i

format l mag divider i
  | m == 0 = l d
  | otherwise = l d ++ divider ++ transform m
  where (d, m) = i `divMod` mag

withMag i = format lhs mask " and " i
  where
    lhs = (++magnitude) . transform
    magnitude = magnitudes mag
    mask = max 100 $ 10^(mag*3)
    mag = pred $ go i
      where
        go 0 = 0
        go i = 1 + go (i `div` 1000)
