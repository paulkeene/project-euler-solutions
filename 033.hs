allFractions :: [(Int, Int, Int, Int)]
allFractions = [(a1, a2, b1, b2) | a1 <- digits, a2 <- digits,
                                   b1 <- digits, b2 <- digits]
    where digits = [1..9]

potentialFractions :: [(Int, Int, Int, Int)]
potentialFractions = filter f allFractions
    where
        f (a1, a2, b1, b2)
            | (a1 == b1) && (a1 /= a2) && (a1 /= b2) && (a2 /= b2) = True
            | (a1 == b2) && (a1 /= a2) && (a1 /= b1) && (a2 /= b1) = True
            | (a2 == b1) && (a2 /= a1) && (a2 /= b2) && (a1 /= b2) = True
            | (a2 == b2) && (a2 /= a1) && (a2 /= b1) && (a1 /= b1) = True
            | otherwise = False

assembleFraction :: (Int, Int, Int, Int) -> (Int, Int)
assembleFraction (a1, a2, b1, b2) = (a1 * 10 + a2, b1 * 10 + b2)

originalValue :: (Int, Int, Int, Int) -> Float
originalValue x = fromIntegral a1 / fromIntegral a2
    where (a1, a2) = assembleFraction x

cancelledValue :: (Int, Int, Int, Int) -> Float
cancelledValue x = (fromIntegral a) / (fromIntegral b)
    where (a, b) = cancelledFraction x

cancelledFraction :: (Int, Int, Int, Int) -> (Int, Int)
cancelledFraction (a1, a2, b1, b2)
    | a1 == b1 = (a2, b2)
    | a1 == b2 = (a2, b1)
    | a2 == b1 = (a1, b2)
    | a2 == b2 = (a1, b1)

isEqual :: Float -> Float -> Bool
isEqual a b = abs (a - b) < 0.00001

specialFractions :: [(Int, Int, Int, Int)]
specialFractions = filter f potentialFractions
    where f x = isEqual (originalValue x) (cancelledValue x) &&
                originalValue x < 1.0

reduceFraction :: (Int, Int) -> (Int, Int)
reduceFraction (a, b) = (a `div` x, b `div` x)
    where x = gcd a b

result :: Int
result = snd. reduceFraction . foldr f (1, 1) $
            map cancelledFraction specialFractions
    where
        f (a1, a2) (acc1, acc2) = (a1 * acc1, a2 * acc2)

main = putStrLn $ "Result = " ++ show result
