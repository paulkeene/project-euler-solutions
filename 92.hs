import qualified Data.List as L
import qualified Data.IntSet as S

type HappyNumbers = S.IntSet
type SadNumbers = S.IntSet

squareDigits :: Int -> [Int]
squareDigits n = n : squareDigits squareSum
  where
    squareSum = sum . map (^ 2) $ toDigits n

toDigits :: Int -> [Int]
toDigits n
  | n < 10 = [n]
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

fromDigits :: [Int] -> Int
fromDigits ns = sum $ zipWith f (reverse ns) [0..]
  where
    f x y = x * 10 ^ y

multsOfXUnderY :: Int -> Int -> Int -> [Int]
multsOfXUnderY x y n = takeWhile (<= y) [n * (x ^ z) | z <- [0..]]

allPerms :: Int -> [Int]
allPerms n = map fromDigits . L.permutations $ toDigits n

categorizeNumber :: Int -> (HappyNumbers, SadNumbers) ->
                    (HappyNumbers, SadNumbers)
categorizeNumber i (happy, sad)
  | S.size sad + S.size happy == threshold = (happy, sad)
  | (i `S.member` sad) || (i `S.member` happy) = (happy, sad)
  | head rest `S.member` sad = (happy, S.union sad (S.fromList numsToAdd))
  | otherwise = (S.union happy (S.fromList numsToAdd), sad)
  where
    sds = squareDigits i
    (chain, rest) = break (\n -> (n `S.member` sad) ||
                                 (n `S.member` happy)) sds
    numsToAdd = concatMap allPerms $ concatMap (multsOfXUnderY 10 threshold)
                chain

threshold :: Int
threshold = 9999999

solution :: [Int]
solution = let (happy, sad) = foldr categorizeNumber
                              (S.fromList [1], S.fromList [89]) [1..threshold]
           in S.toList sad

main :: IO ()
main = do
  print $ length solution
