import Data.List (nub, (\\))

type Digits = [Int]

divisors :: [Int]
divisors = [2, 3, 5, 7, 11, 13]

toDigits :: Int -> Digits
toDigits = map (read . return) . show

toInt :: Digits -> Int
toInt xs = read . foldl (++) "" $ map show xs

prependDigits :: Digits -> [Digits]
prependDigits x = [y : x | y <- [0..9]]

prependMissingDigit :: Digits -> Digits
prependMissingDigit x = missingDigit : x
    where
        missingDigit = ([0..9] \\ x) !! 0

addLeadingZero :: Digits -> Digits
addLeadingZero xs
    | length xs == 2 = 0:xs
    | otherwise = xs

specialMultiples :: Int -> [Digits]
specialMultiples n = map (addLeadingZero . toDigits) $
                         filter pred [n, n*2..n*1000]
    where
        pred x = (len x == 3 || len x == 2) && (nub (show x) == (show x))
        len = length . show

validMultiples :: Int -> Digits -> [Digits]
validMultiples divisor accVal = filter (distinctAndMultiple divisor) $
                                    prependDigits accVal

allValidMultiples :: Int -> [Digits] -> [Digits]
allValidMultiples divisor acc = concat $ map (validMultiples divisor) acc

distinctAndMultiple :: Int -> Digits -> Bool
distinctAndMultiple divisor x = (nub x == x) &&
                                (firstThree `mod` divisor == 0)
    where
        firstThree = toInt $ take 3 x

allValidPandigitals :: [Int]
allValidPandigitals = map toInt . filter noLeadingZero $
                          map prependMissingDigit temp
    where
        temp = foldr allValidMultiples (specialMultiples 17) divisors
        noLeadingZero x = (take 1 x) /= [0]

result :: Int
result = sum allValidPandigitals

main = putStrLn $ "Result: " ++ show result
