{-# LANGUAGE TypeApplications #-}
main = print . digits =<< readFile "in13.txt"
  where
    result = sum . map (read @Integer) . lines
    digits = take 10 . show . result
