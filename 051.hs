import Data.List (nub, subsequences)
import qualified Data.Map as M
import qualified Data.Numbers.Primes as Prime

primes :: [Int]
primes = takeWhile (< 1000000) $ dropWhile (< 13) Prime.primes

counts :: M.Map String [Int]
counts = M.empty

representations :: Int -> [String]
representations x = rs
    where
        str = show x
        sseqs = filter (\y -> (length y /= 0) && (length y /= length str))
                       $ subsequences [0..(length str - 1)]
        f :: [Int] -> Maybe String
        f zs = replaceStr str zs
        ms = map f sseqs
        fMaybe (Just y) ys = y:ys
        fMaybe _ ys = ys
        rs = foldr fMaybe [] ms

replaceStr :: String -> [Int] -> Maybe String
replaceStr s is
    | 1 == (length $ nub cs) = Just $ map f zs
    | otherwise = Nothing
    where
        f :: (Char, Int) -> Char
        f (c, i)
            | i `elem` is = '*'
            | otherwise = c
        zs = zip s [0..(length s - 1)]
        cs = map ((!!) s) is

updateCounts :: [String] -> Int -> M.Map String [Int] ->
                (M.Map String [Int], Bool)
updateCounts reps x cs = foldr f (cs, False) reps
    where
        f :: String -> (M.Map String [Int], Bool) -> (M.Map String [Int], Bool)
        f rep (cs', done) = (updatedCs, done || found8)
            where
                updatedCs = M.insertWithKey g rep [x] cs'
                g :: String -> [Int] -> [Int] -> [Int]
                g k old new = old ++ new
                found8 = case (M.lookup rep updatedCs) of
                    Just y -> 8 == (length y)
                    Nothing -> False

min8Prime :: M.Map String [Int] -> Int
min8Prime cs = minimum . concat $ M.foldWithKey f [] cs
    where
        f :: String -> [Int] -> [[Int]] -> [[Int]]
        f k vs rs
            | length vs >= 8 = vs:rs
            | otherwise = rs

result' :: [Int] -> M.Map String [Int] -> Int
result' (p:ps) cs
    | done = min8Prime cs'
    | otherwise = result' ps cs'
    where
        reps = representations p
        (cs', done) = updateCounts reps p cs

result :: Int
result = result' primes counts

main = putStrLn $ "Result: " ++ (show result)
