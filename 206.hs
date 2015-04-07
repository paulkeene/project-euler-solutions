lowestPossible :: Integer
lowestPossible = floor $ sqrt 1121314151617181910

highestPossible :: Integer
highestPossible = ceiling $ sqrt 1929394959697989990

candidates :: [Integer]
candidates = [lowestPossible, (lowestPossible + 10)..highestPossible]

isValid :: Integer -> Bool
isValid x = (every 2 $ show (x * x)) == "1234567890"

every n (x:xs) = x:(every n (drop (n-1) xs))
every _ [] = []

solution :: Integer
solution =  head $ dropWhile (not . isValid) candidates

main :: IO ()
main = print solution
