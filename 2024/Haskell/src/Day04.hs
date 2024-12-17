module Day04 where

import Lib (count, neighbours, parseAsciiMap)
import Advent.Coord (east, north, south, west, Coord)
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as Map
import Linear (V2(V2))

-- Find all 'X's, look in every direction for 'MAS', count successes
part1 :: Map Coord Char -> Int
part1 input = sum $ do
  s <- Map.keys starts
  pure $ count (\d -> go s d "XMAS") dirs
    where
      dirs = neighbours (V2 0 0)
      starts = Map.filter (== 'X') input
      go pos _ "S" = Map.lookup pos input == Just 'S'
      go pos dir (letter:rest)
        = (Map.lookup pos input == Just letter) && go (pos + dir) dir rest

-- Find every 'A', look for 'M' and 'S' on the diagonals, count successes
part2 :: Map Coord Char -> Int
part2 input = sum $ do
  s <- Map.keys starts
  guard $ nw s == Just 'M' && se s == Just 'S' || nw s == Just 'S' && se s == Just 'M'
  guard $ ne s == Just 'M' && sw s == Just 'S' || ne s == Just 'S' && sw s == Just 'M'
  pure 1
    where
      starts = Map.filter (== 'A') input
      nw pos = Map.lookup (pos + north + west) input
      ne pos = Map.lookup (pos + north + east) input
      se pos = Map.lookup (pos + south + east) input
      sw pos = Map.lookup (pos + south + west) input

main :: IO ()
main = do
  input <- parseAsciiMap Just <$> readFile "../data/day04.in"
  print $ part1 input
  print $ part2 input

-- 2462
-- 1877
