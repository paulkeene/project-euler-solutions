import Data.List
import qualified Data.Map as M

triples :: [(Int, Int, Int)]
triples = [(k * ((m * m) - (n * n)),
            k * (2 * m * n),
            k * ((m * m) + (n * n))) | m <- [1..50],
                                       n <- [1..(m-1)],
                                       k <- [1..50],
                                       pred m n k]
    where
        pred m n k
            | even (m - n) = False
            | gcd m n /= 1 = False
            | ((k*((m*m)-(n*n))) + (k*2*m*n) + (k*((m*m)+(n*n)))) > 1000 = False
            | otherwise = True

maxPerimeter :: [(Int, Int, Int)] -> Int
maxPerimeter triples = fst . maximumBy maxVal . M.toAscList $
                           foldr countPerimeters M.empty triples
    where
        countPerimeters :: (Int, Int, Int) -> (M.Map Int Int) -> (M.Map Int Int)
        countPerimeters (a, b, c) counts = M.insertWith (+) (a + b + c) 1 counts
        maxVal (k1, v1) (k2, v2) = v1 `compare` v2

result = maxPerimeter triples

main = putStrLn $ "Result: " ++ show result
