import Prelude hiding (all)
import Data.Foldable (all)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S

data Rule = Lt Int Int deriving (Show, Eq, Ord)

parseRule :: String -> [Rule]
parseRule (a:b:c:[]) = [Lt a' b',
                        Lt a' c',
                        Lt b' c']
  where
    a' = read [a] :: Int
    b' = read [b] :: Int
    c' = read [c] :: Int

parseRules :: [String] -> S.Set Rule
parseRules ss = S.fromList $ concatMap parseRule ss

toDigits :: Int -> [Int]
toDigits i
  | i < 10 = [i]
  | otherwise = toDigits (i `div` 10) ++ [i `mod` 10]

toDigitMap :: Int -> M.Map Int [Int]
toDigitMap n = Seq.foldlWithIndex f M.empty (Seq.fromList ds)
  where
    ds = toDigits n
    f m i a = M.insertWith (++) a [i] m

satisfiesRule :: M.Map Int [Int] -> Rule -> Bool
satisfiesRule dm (Lt a b) = any f allPairs
  where
    ais = M.findWithDefault [] a dm
    bis = M.findWithDefault [] b dm
    allPairs = [(x, y) | x <- ais, y <- bis]
    f (a, b) = a < b

satisfiesRules :: S.Set Rule -> Int -> Bool
satisfiesRules rs i = all (satisfiesRule dm) rs
  where
    dm = toDigitMap i

shortestPasscode :: S.Set Rule -> Int
shortestPasscode rs = head $ take 1 $ filter (satisfiesRules rs)
                      (candidates [1,2,3,6,7,8,9,0])

candidates :: [Int] -> [Int]
candidates ds = filter f [10000000..]
  where
    f n = dsSet `S.difference` S.fromList (toDigits n) == S.fromList []
    dsSet = S.fromList ds

main :: IO ()
main = do
  content <- readFile "input_files/79_keylog.txt"
  print $ shortestPasscode (parseRules (lines content))
