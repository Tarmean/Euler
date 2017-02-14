import Data.List


main = print $ sum pandigitals
pandigitals = nub $ panRange [1..9] [1000..9999] ++ panRange [10..99] [100..999]
panRange l r = [i*y|i <- l, y <- r, pandigital i y]
pandigital x y = (sort $ show x ++ show y ++ show (x*y)) == ['1'..'9']
