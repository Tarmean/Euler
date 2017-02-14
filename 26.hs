import Data.List
import Data.Ord
import Debug.Trace
main = print $ fst . maximumBy (comparing snd) $ map ((,) <*> countUnique) [1..999]
countUnique origin = count 10 []
  where
    count 0 _ = 0
    count cur seen = let next = cur `mod` origin in
                     case findIndex (==next) seen of
                      Just i -> i+1
                      Nothing -> count (10*next) (next:seen)
