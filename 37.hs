import Data.List
import qualified Data.Numbers.Primes as Prime
import qualified Data.Set as Set

singleDigitPrimes :: Set.Set Int
singleDigitPrimes = Set.fromList [2, 3, 5, 7]

isTPrime :: Int -> Bool
isTPrime x = (Prime.isPrime x) && firstAndLastPrime && (allSubsPrime x) &&
             (not $ Set.member x singleDigitPrimes)
    where
        ds = digits x
        firstAndLastPrime = (Set.member (ds !! 0) singleDigitPrimes) &&
                            (Set.member (ds !! (length ds - 1)) singleDigitPrimes)

allSubsPrime :: Int -> Bool
allSubsPrime = all Prime.isPrime . allSubs

allSubs :: Int -> [Int]
allSubs x = as ++ bs
    where
        ds = digits x
        as = map (read . concat . (map show)) $ filter (/= []) $ inits ds
        bs = map (read . concat . (map show)) $ filter (/= []) $ tails ds

digits :: Int -> [Int]
digits = map (read . return) . show

soln :: [Int] -> [Int] -> [Int]
soln allPrimes@(p:ps) tPrimes
    | length tPrimes == 11 = tPrimes
    | otherwise =
        if isTPrime p
            then soln ps (p:tPrimes)
            else soln ps tPrimes

result :: Int
result = sum $ soln Prime.primes []

main = putStrLn $ "Result: " ++ show  result
