import Data.List ((\\), last)
import qualified Data.Numbers.Primes as Prime

isExpressible :: Int -> Bool
isExpressible n
    | Prime.isPrime n = True
    | even n = True
    | otherwise = [] /= dropWhile (not . Prime.isPrime . fromIntegral) xs
        where
            xs = [g n z | z <- [1..maxZ], g n z > 0]
            maxZ = fromIntegral . floor . toRational . sqrt $ fromIntegral n
            g a b = a - 2 * (b ^ 2)

oddComposite :: Int -> Bool
oddComposite n = odd n && (not $ Prime.isPrime n)

result :: Int
result = head $ dropWhile (\x -> (not $ oddComposite x) || isExpressible x) xs
    where xs = [3,5..]

main = putStrLn $ "Result: " ++ show result
