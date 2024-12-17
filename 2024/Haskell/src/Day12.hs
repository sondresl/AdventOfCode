module Day12 where

import Lib (parseAsciiMap, ordinalNeighbours)
import Advent.Coord (Coord, north, south, east, west)
import Advent.Search (bfs)
import Control.Monad (guard)
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

perimeter mp = Map.mapWithKey f mp
  where f k v = sum $ do
           n <- ordinalNeighbours k 
           guard $ Map.lookup n mp /= Just v
           pure 1

getCost mp
  | Map.null mp = []
  | otherwise   = 
      let (key, val) = Map.findMin mp
          keys = Set.fromList $ bfs [key] (filter ((== Just val) . (`Map.lookup` mp)) . ordinalNeighbours)
       in cost (Map.restrictKeys mp keys) : getCost (Map.withoutKeys mp keys)

cost :: Map Coord Char -> Int
cost mp = Map.size mp * sum (perimeter mp)

getCost2 mp
  | Map.null mp = []
  | otherwise   = 
      let (key, val) = Map.findMin mp
          keys = Set.fromList $ bfs [key] (filter ((== Just val) . (`Map.lookup` mp)) . ordinalNeighbours)
       in part2 (Map.restrictKeys mp keys) : getCost2 (Map.withoutKeys mp keys)

part2 :: Map Coord Char -> Int
part2 mp = (Map.size mp *) $ sum $  map limit [up,down,left,right]
  where
    up :: Map Coord Char
    up    = Map.filterWithKey (\k v -> (k + north) `Map.notMember` mp) mp
    down  = Map.filterWithKey (\k v -> (k + south) `Map.notMember` mp) mp
    left  = Map.filterWithKey (\k v -> (k + west)  `Map.notMember` mp) mp
    right = Map.filterWithKey (\k v -> (k + east)  `Map.notMember` mp) mp
    limit :: Map Coord Char -> Int
    limit vals
      | Map.null vals = 0
      | otherwise     = let (k, v) = Map.findMin vals
                            edge = Set.fromList $ bfs [k] (filter (`Map.member` vals) . ordinalNeighbours)
                   in 1 + limit (Map.withoutKeys vals edge)

main :: IO ()
main = do
  input <- parseAsciiMap Just <$> readFile "../data/day12.in"
  print $ sum $ getCost input
  print $ sum $ getCost2 input
    
-- 1396562
-- 844132
