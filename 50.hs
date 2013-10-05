import qualified Data.Numbers.Primes as Prime
import qualified Data.Set as Set

primes :: [Int]
primes = takeWhile (< 1000000) Prime.primes

sumPrimes :: Int -> Int
sumPrimes n = sum $ take n Prime.primes

primesSet :: Set.Set Int
primesSet = Set.fromList primes

consecPrimeSum :: Int -> Maybe Int
consecPrimeSum n = consecPrimeSum' primes n (sum $ take n primes)

consecPrimeSum' :: [Int] -> Int -> Int -> Maybe Int
consecPrimeSum' ps n s
    | length ps < n = Nothing
    | Set.member s primesSet = Just s
    | otherwise = consecPrimeSum' (drop 1 ps) n (s - (head ps) + (ps !! n))

result :: Int -> Int
result n =
    case x of
        Just a -> a
        Nothing -> result (n-1)
    where
        x = consecPrimeSum n

main = putStrLn $ "Result: " ++ (show $ result 546)
