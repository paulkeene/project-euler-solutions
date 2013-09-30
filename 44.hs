import qualified Data.Set as Set

p :: Int -> Int
p n = (n * (3 * n - 1)) `div` 2

ps :: [Int]
ps = map p [1..5000]

psSet :: Set.Set Int
psSet =  Set.fromList ps

combos :: [(Int, Int)]
combos = [(x, y) | x <- ps, y <- ps, x > y]

special :: (Int, Int) -> Bool
special (x, y) = (Set.member (x + y) psSet) && (Set.member (x - y) psSet)

allSpecial :: [(Int, Int)]
allSpecial = filter special combos

result :: Int
result = minimum $ map (\(x, y) -> x - y) allSpecial

main = putStrLn $ "Result: " ++ show result
