module Day14 where

import Lib (firstRepeatOn, tuple, takeUntil)
import Linear (V2(..), _y)
import Advent.Coord (up, left, right)
import Data.Maybe (maybe)
import Control.Lens (view)
import Data.List.Extra (splitOn, find)
import Data.Set (Set)
import qualified Data.Set as Set

type Rock = Set (V2 Int)

allRock :: [[V2 Int]] -> Rock
allRock xs = Set.fromList $ concatMap (concatMap f . (zip <*> tail)) xs
  where
    f (from, to) = takeUntil (== to) $ iterate (+ dir) from
      where dir = signum $ to - from

run :: (Rock -> V2 Int -> Maybe (V2 Int)) -> Rock -> Rock
run fall rock = go (V2 500 0)
  where
    go pos = maybe rock (`Set.insert` rock) (fall rock pos)

part1 :: Int -> Rock -> V2 Int -> Maybe (V2 Int)
part1 lowestPoint rock p
  | view _y p == lowestPoint = Nothing
  | Just p <- find (`Set.notMember` rock) [up + p, left + up + p, right + up + p] 
    = part1 lowestPoint rock p
  | otherwise = Just p

part2 :: Int -> Rock -> V2 Int -> Maybe (V2 Int)
part2 lowestPoint rock p
  | view _y p == lowestPoint = Just p
  | Just p <- find (`Set.notMember` rock) [up + p, left + up + p, right + up + p] 
    = part2 lowestPoint rock p
  | otherwise = Just p

main :: IO ()
main = do
  input <- allRock . parseInput <$> readFile "../data/day14.in"
  let lowestPoint = Set.findMax $ Set.map (view _y) input
      Just r1 = firstRepeatOn Set.size $ iterate (run (part1 lowestPoint)) input
      Just r2 = firstRepeatOn Set.size $ iterate (run (part2 (lowestPoint + 1))) input
  print $ Set.size r1 - Set.size input
  print $ Set.size r2 - Set.size input

parseInput :: String -> [[V2 Int]]
parseInput = map (map (uncurry V2 . tuple. map read . splitOn ",") . splitOn " -> ") . lines

-- 763
-- 23921
