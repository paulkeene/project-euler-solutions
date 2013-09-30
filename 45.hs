import qualified Data.Set as Set

makeSeq :: (Int -> Int) -> Set.Set Int
makeSeq f = Set.fromList $ map f [143..100000]

result :: Int
result = head $ dropWhile (<= 40755) intersection
    where
        ts = makeSeq (\n -> (n * (n + 1)) `div` 2)
        ps = makeSeq (\n -> (n * (3 * n - 1)) `div` 2)
        hs = makeSeq (\n -> n * (2 * n - 1))
        intersection = Set.toList . Set.intersection ts $ Set.intersection ps hs

main = putStrLn $ "Result: " ++ show result
