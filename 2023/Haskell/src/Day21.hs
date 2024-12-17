module Day21 where

import Lib (findBounds, ordinalNeighbours, parseAsciiMap, Point)
import Advent.Search (bfsOn)
import Control.Monad (guard)
import Data.List.Extra (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V2(V2))

type Garden = Set Point

part1 :: Garden -> Point -> Int
part1 input start = length . filter (even . fst) . takeWhile ((<= 64) . fst) $ walk input (0, start)

walk :: Garden -> (Int, Point) -> [(Int, Point)]
walk input start = bfsOn snd [start] nexts
  where nexts (i, pos) = map (i+1,) . filter (`Set.member` input) $ ordinalNeighbours pos

part2 :: Set Point -> Point -> Int
part2 ps start = totalEven * fullEven
               + totalOdd * fullOdd
               + (bottomLeft + bottomRight + topLeft + topRight) * evenMiddle
               + (missingTopLeft + missingTopRight + missingBottomLeft + missingBottomRight) * oddMiddle
               + fromBottom + fromTop + fromLeft + fromRight
  where
    steps = 26501365 :: Int
    -- Full block count
    oneDir = (steps - 65) `div` 131
    middleRow = (oneDir - 1) * 2 + 1
    evenMiddle = (middleRow `div` 2) + 1
    totalEven = sum [1..evenMiddle - 1] * 2 + evenMiddle
    oddMiddle = middleRow `div` 2
    totalOdd = sum [1..oddMiddle - 1] * 2 + oddMiddle
    -- Rows
    rowsAboveMiddle = oneDir
    -- Stuff
    (minx,miny,maxx,maxy) = findBounds ps
    run start n f = length . filter (f . fst) . takeWhile ((<= n) . fst) $ walk ps (0, start)
    run' start n f = Set.fromList . map snd . filter (f . fst) . takeWhile ((<= n) . fst) $ walk ps (0, start)
    -- Full
    fullOdd = run start 1000 odd
    fullEven = run start 1000 even
    -- corners 
    bottomLeft = run (V2 0 maxy) 64 even
    bottomRight = run (V2 maxx maxy) 64 even
    topLeft = run (V2 0 0) 64 even
    topRight = run (V2 maxx 0) 64 even
    -- Section without one corner (Combine walking from middle of two sides)
    missingBottomLeft = Set.size (run' (V2 65 0) 130 even <> run' (V2 maxx 65) 130 even)
    missingBottomRight = Set.size (run' (V2 65 0) 130 even <> run' (V2 0 65) 130 even)
    missingTopLeft = Set.size (run' (V2 65 maxy) 130 even <> run' (V2 maxx 65) 130 even)
    missingTopRight = Set.size (run' (V2 65 maxy) 130 even <> run' (V2 0 65) 130 even)
    -- Ends (odd/even switched when starting on the edge)
    fromBottom = run (V2 65 maxy) 130 even
    fromTop = run (V2 65 0) 130 even
    fromRight = run (V2 0 65) 130 even
    fromLeft = run (V2 maxx 65) 130 even

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day21.in"
  let ps = Map.keysSet input
      Just (start,_) = find ((== 'S') . snd) $ Map.assocs input
  print $ part1 ps start
  print $ part2 ps start

parseInput :: String -> Map Point Char
parseInput = parseAsciiMap f
  where f x = guard (x `elem` ".S") *> Just x

-- 3809
-- 629720570456311

