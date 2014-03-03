module Main where

import           Control.Monad
import           Data.List
import           Data.List.Split
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Vector as V


type Node = Integer
data Edge = Edge { dest :: Node
                 , cost :: Integer
                 } deriving (Show, Eq)
type Edges = Map.Map Node [Edge]
type Distances = Map.Map Node Integer
type IsVisited = Map.Map Node Bool

computeGraph :: [Integer] -> ([Node], Edges)
computeGraph ints = (nodes', addStart $ V.ifoldl' f Map.empty vs)
  where
    nodes = [0..(nodeCount - 1)]
    nodes' = (-1):nodes
    nodeCount = genericLength ints
    vs = V.fromList ints
    f es i c = es'
      where
        i' = fromIntegral i
        es' = Map.insert i' (getEdges i') es
    getEdges i = catMaybes [rightEdge, downEdge]
      where
        gridWidth = floor . sqrt $ fromIntegral nodeCount
        rightI = i + 1
        rightCost = vs V.! fromIntegral rightI
        inRightColumn = (i /= 1) && (i `mod` gridWidth == (gridWidth - 1))
        rightEdge =
          if (rightI < nodeCount) && not inRightColumn
          then Just Edge {dest=rightI, cost=rightCost}
          else Nothing
        downI = i + gridWidth
        downCost = vs V.! fromIntegral downI
        downEdge =
          if downI < nodeCount
          then Just Edge {dest=downI, cost=downCost}
          else Nothing
    addStart = Map.insert (-1) startEdge
      where
        startEdge = [Edge {dest=0, cost=vs V.! 0}]

getAnswer :: [Integer] -> Integer
getAnswer ints = fromJust . Map.lookup endNode $ findShortestPaths nodes edges ds vs
  where
    (nodes, edges) = computeGraph ints
    ds = Map.empty
    vs = foldl' f Map.empty nodes
    f = flip (`Map.insert` False)
    endNode = last nodes

findShortestPaths :: [Node] -> Edges ->  Distances -> IsVisited -> Distances
findShortestPaths [] _ ds _ = ds
findShortestPaths (n:ns) edges ds vs = findShortestPaths ns edges ds' vs'
  where
    currEdges = case Map.lookup n edges of
      (Just es) -> es
      Nothing -> []
    unvisited = filter isUnvisited currEdges
    isUnvisited edge = not (fromJust (Map.lookup (dest edge) vs))
    currDistance = case Map.lookup n ds of
      (Just d) -> d
      Nothing -> 0
    tds = map (\x -> (dest x, cost x + currDistance)) unvisited
    ds' = updateDistances ds tds
    vs' = Map.insert n True vs

updateDistances :: Distances -> [(Node, Integer)] -> Distances
updateDistances = foldl' f
  where
    f :: Distances -> (Node, Integer) -> Distances
    f ds' (n, d) = Map.insertWith min n d ds'

toIntegers :: String -> [Integer]
toIntegers = map read . concatMap (splitOn ",") . lines

main :: IO ()
main = do
  integers <- liftM toIntegers $ readFile "input_files/81_matrix.txt"
  print $ getAnswer integers
