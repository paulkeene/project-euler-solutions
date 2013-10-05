import qualified Data.Numbers.Primes as Prime

-- might be smart to do some kind of searching algorithm to find the starting point
-- where I pivot over the data and eliminate shit. Sounds kinda like quicksort. A
-- divide and conquor solution might be good.
-- a good way to go about this would be to try to find the first integer that has 4 prime factors and set that as the starting point

distinctPrimeFactors :: Int -> [Int]
distinctPrimeFactors n = primeFactors
    where
        primeFactors = filter (\x -> n `mod` x == 0) $ takeWhile (< n) Prime.primes

result :: Int -> Int
result n
    | len == 0 = n
    | otherwise = result $ n + len
    where
        len = length $ dropWhile pred $ reverse [n..n+3]
        pred x = 4 == (length $ distinctPrimeFactors x)

main = putStrLn $ "Result: " ++ (show $ result 100000)
