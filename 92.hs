import Data.Digits
import qualified Data.IntSet as S

type HappyNumbers = S.IntSet
type SadNumbers = S.IntSet
type Categorization = (HappyNumbers, SadNumbers)

squareDigits :: Int -> [Int]
squareDigits n = n : squareDigits squareSum
  where
    squareSum = sum . map (^ 2) $ digits 10 n

categorizeNumber :: Int -> Categorization -> Categorization
categorizeNumber i (happy, sad)
  | (i `S.member` happy) || (i `S.member` sad) = (happy, sad)
  | head rest `S.member` happy = (S.union happy (S.fromList chain), sad)
  | otherwise = (happy, S.union sad (S.fromList chain))
  where
    (chain, rest) = break (\n -> (n `S.member` sad) || (n `S.member` happy))
                    (squareDigits i)

threshold :: Int
threshold = 10000000

solution :: [Int]
solution = S.toList sad
  where
    (happy, sad) = foldr categorizeNumber (S.fromList [1], S.fromList [89])
                   [1..(threshold - 1)]

main :: IO ()
main = print $ length solution
