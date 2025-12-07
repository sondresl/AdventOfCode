module Day07 where

import Lib (parseAsciiMap, count)
import Advent.Coord (south, west, east, Coord)
import Control.Lens ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

pathMap :: Map Coord Char -> Map Coord [Coord]
pathMap input = flip Map.mapWithKey input $ \k ->
  \case '^' -> [k + west, k + east]
        c   -> [k + south]

part1 :: Map Coord [Coord] -> Map Coord (Set.Set Coord)
part1 input = mp
  where
    mp = flip Map.mapWithKey input $ \k v ->
            case v of
              [x] -> Map.findWithDefault Set.empty x mp
              xs  -> Set.insert k . Set.unions $ map (mp Map.!) v

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
  print . length . (Map.! start) $ part1 mp
  print . (Map.! start) $ part2 mp
    
-- 1541
-- 80158285728929
