import Data.List
import Data.Ord
type Matrix = [[Int]]
main = print . process . parse =<< readFile "in11.txt"

parse ::  String -> Matrix
parse = map parseLine . lines
  where parseLine = map read . words

process :: Matrix -> Int
process ls = maximum  $ maxEntry <$> [ls, transpose ls, diagonals ls, transpose . diagonals $ ls]
  where maxEntry = maximum . concatMap (map product . windows 4)

windows m = takeWhile ((==m).length) . map (take m) . tails 

diagonals = go []
  where go frontier (cur:rest) = map head frontier : go (map tail frontier++[cur]) rest
        go frontier [] = transpose frontier
