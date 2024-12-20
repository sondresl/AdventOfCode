module Day20 where

import Lib (parseAsciiMap, count, ordinalNeighbours, mannDist)
import Advent.Coord (Coord)
import Advent.Search (search)
import Control.Monad (guard)
import Data.List.Extra (tails)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

findCheat :: [(Int, Coord)] -> [Int]
findCheat path = do
  (cost, pos) : rest <- tails path
  (c, p) <- rest
  let md = mannDist pos p
  guard $ md <= 20 && c - cost - md >= 100
  pure md

main :: IO ()
main = do
  (start, mp) <- parseInput <$> readFile "../data/day20.in"
  let cheats = findCheat $ search (map (1,) . filter (`Set.member` mp) . ordinalNeighbours) [start]
  print $ count (== 2) cheats
  print $ length cheats

parseInput :: String -> (Coord, Set Coord)
parseInput input = (start, Map.keysSet mp)
  where 
    mp = parseAsciiMap f input
    start = head [ p | (p, 'S') <- Map.assocs mp ]
    f = \case
       '#' -> Nothing
       any -> Just any

-- 1507
-- 1037936
