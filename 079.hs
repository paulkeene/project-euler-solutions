import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- This implementation assumes that a passcode is comprised of a
-- unique sequence of digits. Allowing duplicate digis in a passcode
-- would break this implementation.

data Rule = Lt Int Int deriving (Show, Eq, Ord)

comparator :: M.Map Int [Int] -> Int -> Int -> Ordering
comparator m a b
  | a == b = EQ
  | a `elem` blt = GT
  | otherwise = LT
  where
    blt = M.findWithDefault [] b m

parseRule :: String -> [Rule]
parseRule [a, b, c] = [Lt a' b',
                       Lt a' c',
                       Lt b' c']
  where
    a' = read [a]
    b' = read [b]
    c' = read [c]

parseRules :: [String] -> S.Set Rule
parseRules ss = S.fromList $ concatMap parseRule ss

distinctDigits :: [String] -> [Int]
distinctDigits input = S.toList $ foldl f S.empty input
  where
    f set line = foldl g set is
      where
        is = map (read . (:[])) line
        g set i = S.insert i set

lessThanMapping :: S.Set Rule -> M.Map Int [Int]
lessThanMapping = S.fold f M.empty
  where
    f (Lt a b) = M.insertWith (++) a [b]

toDigits :: Int -> [Int]
toDigits i
  | i < 10 = [i]
  | otherwise = toDigits (i `div` 10) ++ [i `mod` 10]

fromDigits :: [Int] -> Int
fromDigits is = read . foldl (++) "" $ map show is

shortestPasscode :: [String] -> Int
shortestPasscode input = fromDigits $ L.sortBy (comparator ltm) dds
  where
    rules = parseRules input
    dds = distinctDigits input
    ltm = lessThanMapping rules

main :: IO ()
main = do
  content <- readFile "input_files/79_keylog.txt"
  print . shortestPasscode $ lines content
