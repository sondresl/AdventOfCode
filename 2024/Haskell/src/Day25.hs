module Day25 where

import Lib (parseAsciiMap, count)
import Linear (_x, _y)
import Advent.Coord (Coord, origin)
import Control.Lens (view)
import Control.Monad (guard)
import Data.List.Extra (splitOn, minimumOn, maximumOn, partition)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

heights :: ((Coord -> Int) -> [Coord] -> Coord) -> Set (Coord) -> [Int]
heights comp kl = flip map [0..4] $ \x ->
  view _y
  . comp (view _y)
  . filter ((== x) . view _x)
  $ Set.toList kl

solve :: [[Int]] -> [[Int]] -> Int
solve locks keys = count id $ do
  lock <- locks
  key <- keys
  pure $ and (zipWith (<) lock key)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day25.in"
  let (locks, keys) = input
      locks' = map (heights maximumOn) locks
      keys'  = map (heights minimumOn) keys
  print $ solve locks' keys'
   
parseInput :: String -> ([Set Coord], [Set Coord])
parseInput = partition (origin `Set.member`) . map (Map.keysSet . parseAsciiMap f) . splitOn "\n\n"
  where f c = guard (c == '#') *> Just c

-- 3155
