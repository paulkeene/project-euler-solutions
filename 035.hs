import qualified Data.Numbers.Primes as Primes
import qualified Data.Set as Set

primes :: Set.Set Int
primes = Set.fromList $ take 1000000 Primes.primes

isCircular :: Int -> Bool
isCircular = null . dropWhile (flip Set.member primes) . rotations

rotations :: Int -> [Int]
rotations n = map (read . rotate (show n)) $ [0..((length $ show n) - 1)]

rotate :: String -> Int -> String
rotate xs n = (drop n xs) ++ (take n xs)

result :: Int
result = length . filter isCircular $ Set.toList primes

main = putStrLn $ "Result: " ++ (show result)
