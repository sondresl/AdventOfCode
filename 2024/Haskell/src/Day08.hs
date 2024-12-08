module Day08 where

import Lib (invertMap, select, within, findBounds, parseAsciiMap)
import Advent.Coord (Coord)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  (bounds, ants) <- parseInput <$> readFile "../data/day08.in"

  let antinodes pick xs = Set.fromList $ do
        (a, as) <- select xs
        b <- as
        let diff = a - b
        pick . takeWhile (within bounds) $ iterate (+ diff) a

  print $ Set.size $ foldMap (antinodes (take 1 . drop 1)) ants
  print $ Set.size $ foldMap (antinodes id) ants
    
parseInput :: String -> ((Int, Int, Int, Int), [[Coord]])
parseInput input = (bounds, Map.elems antennas')
  where
    antennas = invertMap $ parseAsciiMap (Just . pure) input
    antennas' = Map.delete '.' antennas
    bounds = findBounds $ antennas Map.! '.'

-- 371
-- 1229
