module Day20 where

import Lib (parseAsciiMap, count, ordinalNeighbours, mannDist)
import Advent.Coord (Coord)
import Advent.Search (search)
import Control.Monad (guard)
import Data.List.Extra (tails)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

findCheat :: [(Int, Coord)] -> Int -> Int
findCheat path n = count (>= 100) $ do
  (cost, pos) : rest <- tails path
  (c, p) <- rest
  guard $ mannDist pos p <= n
  pure $ c - cost - mannDist pos p

main :: IO ()
main = do
  (start, end, mp) <- parseInput <$> readFile "../data/day20.in"
  let path = search (map (1,) . filter (`Set.member` mp) . ordinalNeighbours) [start]
  print $ findCheat path 2
  print $ findCheat path 20

parseInput :: String -> (Coord, Coord, Set Coord)
parseInput input = (start, end, Map.keysSet mp)
  where 
    mp = parseAsciiMap f input
    start = head [ p | (p, 'S') <- Map.assocs mp ]
    end   = head [ p | (p, 'E') <- Map.assocs mp ]
    f = \case
       '.' -> Just '.'
       'S' -> Just 'S'
       'E' -> Just 'E'
       _ -> Nothing

-- 1507
-- 24876416
