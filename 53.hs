fac :: Integer -> Integer
fac 0 = 1
fac n = product [1..n]

nCr :: Integer -> Integer -> Integer
nCr n r = fac n `div` ((fac r) * (fac (n - r)))

result :: Int
result = length $ filter (> (fromIntegral 1000000)) combos
    where
        combos = [nCr n r | n <- [1..100], r <- [1..n]]

main = putStrLn $ "Result: " ++ (show result)
