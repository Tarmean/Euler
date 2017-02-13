import Data.List
import Data.Ord
main = print . score . lines =<< readFile "in22.txt"

score = sum . zipWith (*) [1..] . map nameValue
nameValue = sum . map charValue
  where charValue = succ . (subtract $ fromEnum 'A') . fromEnum
