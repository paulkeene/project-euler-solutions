module Main where

import Prelude hiding (foldr)

import Control.Applicative ((<*>))
import Data.Char (digitToInt, intToDigit)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe, fromJust)
import Data.Functor ((<$>))
import Data.List (sortBy)
import Data.List.Split (chunksOf)
import qualified Data.Set as S
import Data.Foldable (foldr)

type UnsolvedPuzzle = Map (Char, Int) (Maybe Int)
type SolvedPuzzle = Map (Char, Int) Int

gridSize :: Int
gridSize = 9

showUnsolvedPuzzle :: UnsolvedPuzzle -> String
showUnsolvedPuzzle puzzle = unlines $ "Puzzle\n":rows
  where
    rows = map f [1..9]
    f r = map (g r) "ABCDEFGHI"
    g r c = case fromJust v of
            Nothing -> '0'
            Just x -> intToDigit x
      where
        v = M.lookup (c, r) puzzle

parsePuzzles :: [String] -> [UnsolvedPuzzle]
parsePuzzles = map parsePuzzle . chunksOf 10

parsePuzzle :: [String] -> UnsolvedPuzzle
parsePuzzle strs = M.fromList values
  where
    rows :: [(Int, String)]
    rows = zip [1..9] (drop 1 strs)
    values :: [((Char, Int), Maybe Int)]
    values = foldr f [] rows
    f :: (Int, String) -> [((Char, Int), Maybe Int)] -> [((Char, Int), Maybe Int)]
    f (r, row) xs = ys ++ xs
      where
        ys :: [((Char, Int), Maybe Int)]
        ys = zipWith (curry g) "ABCDEFGHI" row
        g (c, '0') = ((c, r), Nothing)
        g (c, value) = ((c, r), Just $ digitToInt value)

solve :: UnsolvedPuzzle -> Maybe SolvedPuzzle
solve puzzle
  | isComplete puzzle = mkSolvedBoard puzzle
  | otherwise = solution
  where
    possibleSolutions :: [SolvedPuzzle]
    possibleSolutions = mapMaybe solve (nextSteps puzzle)
    solution = case take 1 possibleSolutions of
               [] -> Nothing
               (x:_) -> Just x

nextSteps :: UnsolvedPuzzle -> [UnsolvedPuzzle]
nextSteps puzzle = f possibleValues
  where
    f :: ((Char, Int), [Int]) -> [UnsolvedPuzzle]
    f (k, vs) = map (flip (M.insert k) puzzle . Just) vs
    possibleValues = head $ orderedPossibleValuesForEmpties puzzle

orderedPossibleValuesForEmpties :: UnsolvedPuzzle -> [((Char, Int), [Int])]
orderedPossibleValuesForEmpties puzzle = results
  where
    empties :: [(Char, Int)]
    empties = M.keys $ M.filter (== Nothing) puzzle
    possibilities :: [((Char, Int), [Int])]
    possibilities = map (\x -> (x, possibleValuesForCell puzzle x)) empties
    comparator :: (a, [b]) -> (a, [b]) -> Ordering
    comparator (_, xs) (_, ys) = compare (length xs) (length ys)
    results = sortBy comparator possibilities


possibleValuesForCell :: UnsolvedPuzzle -> (Char, Int) -> [Int]
possibleValuesForCell puzzle c = S.toList $ S.difference allValues takenValues
  where
    allValues = S.fromList [1..9]
    columnValues = S.fromList $ columnValuesForCell puzzle c
    rowValues = S.fromList $ rowValuesForCell puzzle c
    squareValues = S.fromList $ squareValuesForCell puzzle c
    takenValues = S.union (S.union columnValues rowValues) squareValues

columnValuesForCell :: UnsolvedPuzzle -> (Char, Int) -> [Int]
columnValuesForCell puzzle (c, _) = catMaybes . M.elems $ M.filterWithKey f puzzle
  where
    f (c', _) _ = c' == c

rowValuesForCell :: UnsolvedPuzzle -> (Char, Int) -> [Int]
rowValuesForCell puzzle (_, r) = catMaybes . M.elems $ M.filterWithKey f puzzle
  where
    f (_, r') _ = r' == r

squareValuesForCell :: UnsolvedPuzzle -> (Char, Int) -> [Int]
squareValuesForCell puzzle (c, r) = catMaybes . M.elems $ M.filterWithKey f puzzle
  where
    f (c', r') _ = (columnBucket c == columnBucket c') &&
                   (rowBucket r == rowBucket r')

columnBucket :: Char -> Int
columnBucket c
  | c <= 'C' = 1
  | c <= 'F' = 2
  | otherwise = 3

rowBucket :: Int -> Int
rowBucket r
  | r <= 3 = 1
  | r <= 6 = 2
  | otherwise = 3

isComplete :: UnsolvedPuzzle -> Bool
isComplete p = M.filter (== Nothing) p == M.empty

mkSolvedBoard :: UnsolvedPuzzle -> Maybe SolvedPuzzle
mkSolvedBoard p
  | M.filter (== Nothing) p == M.empty = Just $ M.map fromJust p
  | otherwise = Nothing

computeSum :: [SolvedPuzzle] -> Maybe Int
computeSum ps = sum <$> mapM topLeftNumber ps

topLeftNumber :: SolvedPuzzle -> Maybe Int
topLeftNumber m = (+) <$> ((+) <$> x <*> y) <*> z
  where
    x :: Maybe Int
    x = (100 *) <$> M.lookup ('A', 1) m
    y = (10 * ) <$> M.lookup ('B', 1) m
    z = M.lookup ('C', 1) m

main :: IO ()
main = do
  content <- readFile "input_files/096_sudoku.txt"
  let unsolvedPuzzles = parsePuzzles $ lines content
  print $ computeSum <$> mapM solve unsolvedPuzzles
