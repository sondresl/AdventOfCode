module Day10 where

import Lib
    (Point, ordinalNeighbours, zipWithTail', findBounds, neighbours, parseAsciiMap)
import Advent.Search (bfs, dfs)
import Advent.Coord (up, down, left, right)
import Data.Maybe (fromJust, isJust)
import Control.Monad (guard)
import Data.List.Extra (find, unfoldr)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V2(V2))

isNeighbour :: Map Point Char -> Point -> Point -> Bool
isNeighbour mp pos cand
  | pos + up == cand = val `elem` "|LJ" && curr `elem` "SF|7"
  | pos + down == cand = val `elem` "|7F" && curr `elem` "S|LJ"
  | pos + left == cand = val `elem` "-LF" && curr `elem` "S-7J"
  | pos + right == cand = val `elem` "-7J" && curr `elem` "S-LF"
  where 
    val = mp Map.! cand 
    curr = mp Map.! pos

part2 :: Map Point Char -> Int
part2 input = Set.size $ Set.filter (`Set.member` double) res
  where
    Just res = find (not . any onRim) $ unfoldr findInternal initial
    double = Set.mapMonotonic (*2) $ Map.keysSet input
    path = Set.fromList . concatMap fix . zipWithTail' . map (*2) $ mkPath input
    fix (V2 x y, V2 a b) = [V2 x y, V2 ((x + a) `div` 2) ((y + b) `div` 2)]
    (subtract 1 -> minX, subtract 1 -> minY, (+1) -> maxX, (+1) -> maxY) = findBounds double
    onRim (V2 x y) = minX == x || x == maxX || minY == y || y == maxY
    initial = (`Set.difference` path) $ Set.fromList $ liftA2 V2 [minX..maxX] [minY..maxY]
    findInternal starts = (found,) <$> (guard (isJust start) *> Just (starts `Set.difference` found))
      where
        start = Set.lookupMin starts
        f curr = filter (`Set.member` starts) $ neighbours curr
        found = Set.fromList $ bfs [fromJust start] f

mkPath :: Map Point Char -> [Point]
mkPath input = dfs id generateNext start
  where
    Just start = fst <$> find ((== 'S') . snd) (Map.assocs input)
    generateNext x = filter (liftA2 (&&) (`Map.member` input) (isNeighbour input x)) $ ordinalNeighbours x

main :: IO ()
main = do
  input <- parseAsciiMap Just <$> readFile "../data/day10.in"
  print $ (`div` 2) . length $ mkPath input
  print $ part2 input

-- 6640
-- 411
