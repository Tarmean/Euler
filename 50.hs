import Data.Numbers.Primes
import Data.List
import Data.Ord
import Control.Applicative
import Control.Arrow
import qualified Data.IntSet as S

main = print . snd $ maximumBy (comparing fst) primeSums

primeSums = (filter (isPrimeM . snd) . takeWhile ((<=high) . snd) . map (length &&& sum) . tail . inits) =<< tails primesM
  where
    primesM = takeWhile (<1000000) primes
    primeSet = S.fromList primesM
    isPrimeM = (`S.member` primeSet)
    high = last primesM

