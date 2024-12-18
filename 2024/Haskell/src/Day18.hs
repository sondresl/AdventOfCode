module Day18 where

import Lib (binaryMinSearch, allNums, tuple, ordinalNeighbours)
import Advent.Coord (Coord, origin)
import Advent.Search (search)
import Data.Maybe (isNothing, fromJust, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V2(V2))

shortestPath :: [Coord] -> Int -> Maybe Int
shortestPath input n = listToMaybe [ l | (l, V2 70 70) <- search go [origin] ]
  where
    blocks = Set.fromList $ take n input
    inside (V2 x y) = x <= 70 && x >= 0 && y <= 70 && y >= 0
    go pos = map (1,) . filter inside . filter (`Set.notMember` blocks) $ ordinalNeighbours pos

main :: IO ()
main = do
  input <- map (uncurry V2 . tuple . allNums) . lines <$> readFile "../data/day18.in"
  print . fromJust $ shortestPath input 1024
  let Just firstFailure = binaryMinSearch (isNothing . shortestPath input) 1025 (length input)
  putStrLn . (\(V2 x y) -> show x <> "," <> show y) $ input !! (firstFailure - 1)

-- 344
-- 46,18
