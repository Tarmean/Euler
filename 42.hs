import qualified Data.IntSet as S
value = sum . map ord
  where ord = succ . subtract (fromEnum 'A') . fromEnum

main = print . length . filter isTriangle . lines =<< readFile "in42.txt"
  where isTriangle = (`S.member` triangleSet) . value

triangleSet = S.fromList $ takeWhile (<= 192) $ scanl1 (+) [1..]
