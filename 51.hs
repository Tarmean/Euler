import qualified Data.Numbers.Primes as P
import qualified Data.IntSet as S
main = print $ head findSolution

-- Originally I also had conditions that checked whether the sum of unchanging digits was
-- divisible by three and whether the last digit was changing but all these [Char] -> Int
-- steps actually slowed the solution down.
findSolution = filter ((&&) <$> checkVariables <*> checkFamily) candidates
  where
    candidates = [100003,100005..999999]
    checkVariables = (==3) . count '1' . show
    checkFamily = (==8) . length . primeFamily

primeFamily = filter isPrime . family
family num = replaceWith <$> ['1'..'9']
  where
    digits = show num
    replaceWith a = read $ step a <$> digits
    step a '1'   = a
    step _ other = other

primesS = S.fromList $ takeWhile (<999999) P.primes
isPrime = (`S.member` primesS)

count = (length.) . filter . (==)
