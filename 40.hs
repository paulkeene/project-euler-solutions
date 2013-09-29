import qualified Data.Numbers.Primes as Primes
import Data.List

pPrimes :: Int -> [Int]
pPrimes n = filter Primes.isPrime $ map read pandigitals
    where
        pandigitals = permutations ['1'..((show n) !! 0)]

result :: Int
result = maximum . concat $ map pPrimes [1..9]
