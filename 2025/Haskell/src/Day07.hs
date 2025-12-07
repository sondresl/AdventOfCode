module Day07 where

import Lib
import Advent.Coord
import Advent.Search
import Control.Lens ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map

part1 input = bfs starts nexts
  where
    starts = Map.keys $ Map.filter (== 'S') input
    nexts pos = let new = south + pos
                 in case Map.lookup new input of
                      Just '.' -> [new]
                      Just '^' -> [west + pos, east + pos]
                      Nothing  -> []

pathMap :: Map Coord Char -> Map Coord [Coord]
pathMap input = flip Map.mapWithKey input $ \k ->
  \case '^' -> [k + west, k + east]
        c   -> [k + south]

part2 :: Map Coord [Coord] -> Map Coord Int
part2 input = mp
  where
    mp = input <&> \xs ->
            sum $ map (\pos -> Map.findWithDefault 1 pos mp) xs

main :: IO ()
main = do
  input <- parseAsciiMap Just <$> readFile "../data/day07.in"
  let start = head . Map.keys $ Map.filter (== 'S') input
      mp = pathMap input
  print $ count ((== Just '^') . (`Map.lookup` input) . (+ south)) $ part1 input
  let p2 = part2 $ pathMap input
  print $ Map.lookup start p2
    
-- 1541
-- Just 80158285728929
