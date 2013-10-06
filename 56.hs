digits :: Integer -> [Integer]
digits x = map (read . return) $ show x

sumDigits :: Integer -> Integer
sumDigits = sum . digits

result :: Integer
result = maximum [sumDigits (a ^ b) | a <- xs, b <- xs]
    where
        xs = [(fromIntegral 1)..(fromIntegral 100)]
