module Day16 where

import Lib (Point, findBounds, parseAsciiMap)
import Advent.Search (bfs)
import Advent.Coord (up, down, right, left)
import Control.Monad (guard)
import Data.List.Extra (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Linear (V2(V2))

type Light = (Point, Point) -- Position, Direction

part1 :: Map Point Char -> Light -> Int
part1 input = length . nub . map fst . simulate input

simulate :: Map Point Char -> Light -> [Light]
simulate input start = tail $ bfs [start] (light input)

part2 :: Map Point Char -> [Int]
part2 input = do
    let overX = map (\x -> part1 input (V2 x (-1), up)) [0..maxx] 
    let underX = map (\x -> part1 input (V2 x (maxy + 1), down)) [0..maxx]
    let leftY = map (\y -> part1 input (V2 (-1) y, right)) [0..maxy]
    let rightY = map (\y -> part1 input (V2 (maxx + 1) y, left)) [0..maxy]
    overX <> underX <> leftY <> rightY
  where (minx, miny, maxx, maxy) = findBounds (Map.keysSet input)

newDirection :: Char -> Point -> [Point]
newDirection mirror = case mirror of
  '.' -> pure
  '/' -> \case
    V2 0 1 -> [left]
    V2 0 (-1) -> [right]
    V2 (-1) 0 -> [up]
    V2 1 0 -> [down]
  '\\' -> \case
    V2 0 1 -> [right]
    V2 0 (-1) -> [left]
    V2 (-1) 0 -> [down]
    V2 1 0 -> [up]
  '|' -> \case
    V2 0 1 -> [up]
    V2 0 (-1) -> [down]
    V2 (-1) 0 -> [up, down]
    V2 1 0 -> [up, down]
  '-' -> \case
    V2 0 1 -> [left, right]
    V2 0 (-1) -> [left, right]
    V2 (-1) 0 -> [left]
    V2 1 0 -> [right]
    
light :: Map Point Char -> Light -> [Light]
light mp (pos, dir) =
  case Map.lookup (pos + dir) mp of
    Nothing -> []
    Just next -> map (pos + dir,) $ newDirection next dir

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day16.in"
  let res = part1 input (V2 (-1) 0, right)
  print res
  let res = part2 input
  print $ maximum res

parseInput :: String -> Map Point Char
parseInput = parseAsciiMap f
  where f x = guard (x `elem` ".\\/|-") *> Just x
--
-- 7496
-- 7932

