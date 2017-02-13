{-# LANGUAGE TypeApplications #-}
main = print . digits =<< readFile "in13.txt"
  where
    result = sum . map (read @Integer) . lines
    digits = take 10 . show . result

ones = (!!) ["" ,"one" ,"two" ,"three" ,"four" ,"five" ,"six" ,"seven" ,"eight" ,"nine"]
tens = (!!) ["" ,"teen" ,"twenty" ,"thirty" ,"forty" ,"fifty" ,"sixty" ,"seventy" ,"eighty" ,"ninety"]
magnitudes = (!!) ["" ," thousand" ," million" ," billion"]

inEnglish :: Int -> Maybe String
inEnglish i
  -- | i < 0 = ("minus " ++) <$> inEnglish (-i)
  | i < 0 = Nothing
  | i == 0 = Just "zero"
  | otherwise = Just $ unwords $ process digits
  where 
    digits = map digitToInt $ show i

    process = filter (not . null) . reverse . map block . chunks
    chunks =  zip [0..] . map reverse . chunksOf 3 . reverse
    block (m, ls)
      | all (==0) ls = ""
      | otherwise    = printf "%s%s" (segment ls) (magnitudes m)

    segment [i] = ones i

    segment [1, 0] = printf "ten"
    segment [1, 1] = printf "eleven"
    segment [1, 2] = printf "twelve"
    segment [1, 3] = printf "thirteen"
    segment [1, i] = printf "%steen" (ones i)
    segment [0, i] = printf "%s" (ones i)
    segment [d, 0] = printf "%s" (tens d)
    segment [d, i] = printf "%s-%s" (tens d) (ones i)

    segment [c, 0, 0] = printf "%s hundred" (ones c)
    segment (0:ls) = segment ls
    segment (c:ls) = printf "%s hundred %s" (ones c) (segment ls)
