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

dijkstra :: Ord a => Map a [(a, Int)] -> a -> Map a Int
dijkstra graph start = go seen Map.empty
  where
    seen = Map.insert start (Just 0) . fmap (const Nothing) $ graph
    go waiting result
      | Map.null waiting = result
      | otherwise = 
        let next@(node, Just value) = 
              minimumBy (compare `on` (fromJust . snd)) $ filter (isJust . snd) $ Map.toList waiting
            targets = filter ((`elem` Map.keys waiting) . fst) $ graph Map.! fst next
            add weight Nothing = Just $ weight + value
            add weight (Just y) = Just $ min y (weight + value)
            new = foldr (\(k, v) acc -> Map.adjust (add v) k acc) waiting targets
         in go (Map.delete node new) (Map.insert node value result)

search :: Ord a => (a -> [(Int, a)]) -> [a] -> [(Int, a)]
search nexts starts = go Set.empty begin
  where
    begin = MinQ.fromList $ map (0,) starts
    go seen = \case
      Empty -> []
      (cost, val) :< rest
        | Set.member val seen -> go seen rest
        | otherwise -> (cost, val) : go seen' work
        where
          seen' = Set.insert val seen
          work = foldl' insertWork rest (nexts val)
          insertWork qu (edge, val) = MinQ.insert (cost + edge, val) qu

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

