module Day08 where

import Lib (within, findBounds, parseAsciiMap, combinations)
import Advent.Coord (Coord)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear

data Location = Space | Antenna Char
  deriving (Show, Eq)

antinodes :: ([Coord] -> [Coord]) -> (Int, Int, Int, Int) -> [Coord] -> Set Coord
antinodes f bounds cs = Set.fromList $ do
  [a, b] <- combinations 2 cs
  let diff = a - b
      one = f . takeWhile (within bounds) $ iterate (+ diff) a
      two = f . takeWhile (within bounds) $ iterate (subtract diff) b
  one <> two

main :: IO ()
main = do
  (bounds, ants) <- parseInput <$> readFile "../data/day08.in"
  print $ Set.size $ foldMap (antinodes (take 1 . drop 1) bounds) ants
  print $ Set.size $ foldMap (antinodes id bounds) ants
    
parseInput :: String -> ((Int, Int, Int, Int), [[Coord]])
parseInput input = (bounds, Map.elems ants')
  where
    ants = parseAsciiMap Just input
    bounds = findBounds [ pos | (pos, _) <- Map.assocs ants ]
    ants' = Map.fromListWith (<>) [ (c, [pos]) | (pos, c) <- Map.assocs ants 
                                               , c /= '.' ]
-- 371
-- 1229
