import Data.List (nub, sort)

sameDigits :: Int -> Int -> Bool
sameDigits x y = f x == (f y)
    where f = sort . show

same1Through6 :: Int -> Bool
same1Through6 x =  5 == (length $ takeWhile (sameDigits x) [x * y | y <- [2..6]])

result :: Int
result = head $ dropWhile (not . same1Through6) [100..]

main = putStrLn $ "Result: " ++ (show result)
