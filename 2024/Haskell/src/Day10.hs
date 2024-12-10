module Day10 where

import Lib (parseAsciiMap, ordinalNeighbours)
import Advent.Coord (Coord)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Semigroup

solve :: Monoid m => Map Coord Int -> (Coord -> m) -> Coord -> m
solve input f = go 0
  where
    go 9 pos = f pos
    go v pos = foldMap (go (v + 1)) next
      where next = [ p | p <- ordinalNeighbours pos, Map.lookup p input == Just (v + 1)]

main :: IO ()
main = do
  input <- parseAsciiMap (Just . read . pure) <$> readFile "../data/day10.in"
  let starts = [ k | (k, 0) <- Map.assocs input ]
  print $ sum $ map (Set.size . solve input Set.singleton  ) starts
  print $ sum $ map (getSum   . solve input (const (Sum 1))) starts

-- 811
-- 1794
