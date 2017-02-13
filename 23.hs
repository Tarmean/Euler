import Data.List
import Data.Numbers.Primes
import qualified Data.IntSet as S
main = print $ sum notProperSums
notProperSums = filter (`S.notMember` properSums) [1..28123]
properSums = S.fromList [x+y|x<- properNums, y<-properNums]
properNums = filter ((>) =<< properSum) [1..28123]
properSum = (-) =<< product . map (sum . scanl (*) 1) . group . primeFactors
