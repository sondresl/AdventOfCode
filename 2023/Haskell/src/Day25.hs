module Day25 where

import Lib (select)
import Advent.Search (bfs)
import Control.Monad (guard)
import Data.List.Extra (delete, nub, find, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Graph (components, graphFromEdges)

solve :: Map String [String] -> Int
solve input = let [a,b] = components graph in length a * length b
  where
    res = minCut input
    mp = foldl (\acc (from, to) -> Map.adjust (delete to) from $ Map.adjust (delete from) to acc) input res
    (graph,_,_) = graphFromEdges $ do
      k <- Map.keys mp
      let to = mp Map.! k
      pure (k, k, to)

-- Remove one edge, and find the new shortest distance between the nodes. If it is >6,
-- include that edge. 6 was found to be the correct number to get exactly 3 edges back.
minCut :: Map String [String] -> [(String, String)]
minCut mp = nub $ do
  k <- Map.keys mp
  let ts = mp Map.! k
  (t, rest) <- select ts
  -- BFS To the other neibhgours of k?
  let Just (d,res) = find (\(i,f) -> f `elem` rest) $ bfs [(0,t)] (\(i,f) -> map (i+1,) . filter (/= k) $ mp Map.! f)
  guard $ d > 6 -- Just tested a number that returned exactly three edges
  pure (min k t, max k t)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day25.in"
  print $ solve input

parseInput :: String -> Map String [String]
parseInput = Map.unionsWith (<>) . map (f . splitOn ": ") . lines
  where 
    f [from, words -> to] = Map.unionsWith (<>) $ do
      t <- to
      pure $ Map.singleton t [from] <> Map.singleton from [t]

-- 558376
