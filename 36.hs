main = print . sum $ filter doublePalim [1..1000000]
doublePalim = (&&) <$> palim . show <*> palim . toRevBin
palim :: Eq a => [a] -> Bool
palim = (==) =<< reverse

toRevBin 0 = []
toRevBin dec = d : toRevBin m
  where (m, d) = dec `divMod` 2

