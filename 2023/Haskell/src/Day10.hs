module Day10 where

import Lib
    (Point, bfs, ordinalNeighbours, zipWithTail', findBounds, neighbours, dfs, parseAsciiMap)
import Advent.Coord (up, down, left, right)
import Data.Maybe (fromJust, isJust)
import Control.Monad (guard)
import Data.List.Extra (find, unfoldr)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V2(V2))

findDistance :: Map Point Char -> [(Int, Point)]
findDistance input = bfs [(0, start)] f snd
  where
    Just start = fst <$> find ((== 'S') . snd) (Map.assocs input)
    f (n, x) = map (n + 1,)
             . filter (isNeighbour input x) 
             . filter (`Map.member` input)
             $ ordinalNeighbours x

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
        found = Set.fromList $ bfs [fromJust start] f id

mkPath :: Map Point Char -> [Point]
mkPath input = dfs id f start
  where
    Just start = fst <$> find ((== 'S') . snd) (Map.assocs input)
    f x = filter (isNeighbour input x) . filter (`Map.member` input) $ ordinalNeighbours x

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day10.in"
  print $ maximum . map fst $ findDistance input
  print $ part2 input

parseInput :: String -> Map Point Char
parseInput = parseAsciiMap f
  where f x = guard (x `elem` "|-LJ7FS.") *> Just x

-- 6640
-- 411
