module Advent.Search where

import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq, empty)
import qualified Data.PQueue.Min as MinQ
import           Data.PQueue.Min (MinQueue(..))
import Data.Foldable (Foldable(foldl'))
import Data.Maybe (fromJust, isJust)
import Data.Function (on)
import Data.List.Extra (minimumBy)
import Advent.Coord (Coord)

dijkstra :: Ord a => (a -> [(Int, a)]) -> [a] -> Map a Int
dijkstra nexts starts = Map.fromList $ go Set.empty begin
  where
    begin = MinQ.fromList $ map (0,) starts
    go seen = \case
      Empty -> []
      (cost, val) :< rest
        | Set.member val seen -> go seen rest
        | otherwise -> (val, cost) : go seen' work
        where
          seen' = Set.insert val seen
          work = foldl' insertWork rest (nexts val)
          insertWork qu (edge, val) = MinQ.insert (cost + edge, val) qu


search :: Ord a => (a -> [(Int, a)]) -> [a] -> [(Int, a)]
search = searchOn id

-- Basically A*, but without heuristic, or dijkstra but not returning a map
searchOn :: 
  (Ord a, Ord b) => 
  (a -> b) -> -- Repr function for the set of seen elements
  (a -> [(Int, a)]) ->  -- Function for genrating neighbours from a given node, with weight of that single step
  [a] -> -- Starting positions
  [(Int, a)] -- Weight and value
searchOn repr nexts starts = go Set.empty begin
  where
    begin = MinQ.fromList $ map (0,) starts
    go seen = \case
      Empty -> []
      (cost, val) :< rest
        | Set.member rep seen -> go seen rest
        | otherwise -> (cost, val) : go seen' work
        where
          rep = repr val
          seen' = Set.insert rep seen
          work = foldl' insertWork rest (nexts val)
          insertWork qu (edge, val) = MinQ.insert (cost + edge, val) qu

-- allPathsOn :: (Ord a, Ord b) => [a] -> (a -> [a]) -> Map (a, a) [a]
-- allPaths = allPathsOn id

-- allPathsOn :: -- Find all paths from a start node to all other nodes
--   (Ord a, Ord b) =>
--   (a -> b) -> -- Represent in set of seen
--   [a] -> -- Initial candidates
--   (a -> [a]) -> -- Generate new candidates from current
--   Map (a, a) [a] -- Map of paths between start and any a
-- allPathsOn repr start fn = go Set.empty (Map.singleton (start, start) []) (Seq.fromList start)
--   where
--     go seen paths next
--       | Seq.null next = paths
--       | otherwise = let (c Seq.:<| cs) = next
--       in let cands = filter (not . (`Set.member` seen) . repr) $ fn c
--              seen' = seen <> Set.fromList (map repr cands)
--              paths
--        in c : go (Set.insert (repr c) seen') (cs Seq.>< Seq.fromList cands)

bfs :: 
  (Ord a) =>
  [a] -> -- Initial candidates
  (a -> [a]) -> -- Generate new candidates from current
  [a] -- All the visited 'areas'
bfs = bfsOn id

bfsOn ::
  (Ord a, Ord b) =>
  (a -> b) -> -- Represent in set of seen
  [a] -> -- Initial candidates
  (a -> [a]) -> -- Generate new candidates from current
  [a] -- All the visited 'areas'
bfsOn repr start fn = go Set.empty (Seq.fromList start)
  where
    go seen next
      | Seq.null next = []
      | otherwise = let (c Seq.:<| cs) = next
      in let cands = filter (not . (`Set.member` seen) . repr) $ fn c
             seen' = seen <> Set.fromList (map repr cands)
       in c : go (Set.insert (repr c) seen') (cs Seq.>< Seq.fromList cands)

dfs :: (Ord r, Ord a) => (a -> r) -> (a -> [a]) -> a -> [a]
dfs repr next start = go Set.empty [start]
  where
    go _ [] = []
    go seen (x:xs)
      | Set.member r seen = go seen xs
      | otherwise = x : go (Set.insert r seen) (next x <> xs)
     where
       r = repr x

floodFill ::
  (Coord -> [Coord]) -> -- Generate members of this 'flood'
  Set Coord -> -- Map
  [Set Coord] -- All the different 'floods'
floodFill generate = go
  where
    go mp | Set.null mp = []
    go mp = let k = Set.findMin mp
                group = Set.fromList $ bfs [k] generate
             in group : go (Set.difference mp group)

