module Day12 where

import Lib (parseAsciiMap, ordinalNeighbours, count)
import Advent.Coord (Coord, north, south, east, west)
import Advent.Search (floodFill)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

gardens :: Map Coord Char -> [Set Coord]
gardens mp = flip floodFill (Map.keysSet mp) $ \key ->
    filter ((== key `Map.lookup` mp) . (`Map.lookup` mp)) $ ordinalNeighbours key

perimeter :: Set Coord -> Int
perimeter mp = foldr f 0 mp
  where f k acc = acc + count (`Set.notMember` mp) (ordinalNeighbours k)

sides :: Set Coord -> Int
sides mp = sum $ map (edges . dirs) [north, south, east, west]
  where 
    dirs dir = Set.filter (\k -> (k + dir) `Set.notMember` mp) mp
    edges vals = length $ floodFill (filter (`Set.member` vals) . ordinalNeighbours) vals

main :: IO ()
main = do
  input <- parseAsciiMap Just <$> readFile "../data/day12.in"
  let solve f = sum . map (\mp -> Set.size mp * f mp) . gardens
  print $ solve perimeter input
  print $ solve sides input
    
-- 1396562
-- 844132
