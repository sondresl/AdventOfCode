module Day12 where

import Lib (parseAsciiMap, ordinalNeighbours, count)
import Advent.Coord (Coord, north, south, east, west)
import Advent.Search (floodFill)
import Data.Map (Map)
import qualified Data.Map as Map

gardens :: Map Coord Char -> [Map Coord Char]
gardens mp = flip floodFill mp $ \key ->
    filter ((== key `Map.lookup` mp) . (`Map.lookup` mp)) $ ordinalNeighbours key

perimeter :: Map Coord Char -> Int
perimeter mp = sum $ flip Map.mapWithKey mp $ \k v ->
  count ((Just v /=) . (`Map.lookup` mp)) (ordinalNeighbours k)

sides :: Map Coord Char -> Int
sides mp = sum $ map (edges . dirs) [north, south, east, west]
  where 
    dirs dir = Map.filterWithKey (\k _ -> (k + dir) `Map.notMember` mp) mp
    edges vals = length $ floodFill (filter (`Map.member` vals) . ordinalNeighbours) vals

main :: IO ()
main = do
  input <- parseAsciiMap Just <$> readFile "../data/day12.in"
  let solve f = sum . map (\mp -> Map.size mp * f mp) . gardens
  print $ solve perimeter input
  print $ solve sides input
    
-- 1396562
-- 844132
