module Day17 where

import Lib (Point, findBounds, ordinalNeighbours, parseAsciiMap)
import Advent.Search (search)
import Advent.Coord (origin)
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as Map
import Linear (V2(V2)) 

data Direction = North | South | East | West
  deriving (Show, Eq, Ord)

solve :: Map Point [(Point, Int)] -> Int -> Int -> Int
solve input lo hi = head [ dist
                         | (dist, (pos, _, steps)) <- search (goNext lo hi input) starts
                         , pos == V2 maxx maxy
                         , steps >= lo]
  where 
    (_,_,maxx,maxy) = findBounds (Map.keysSet input)
    starts = [(origin, East, 0), (origin, South, 0)] -- search (mkGraph input) (V2 0 0) (V2 maxx maxy)

goNext :: Int -> Int -> Map Point [(Point, Int)] -> (Point, Direction, Int) -> [(Int, (Point, Direction, Int))]
goNext minMove maxMove mp (pos, dir, steps) = targets
  where
    targets = do
      t <- mp Map.! pos
      let res@(_,(_,dir',st)) = f t
      guard $ (dir' == dir) || (steps >= minMove)
      guard $ st <= maxMove
      guard $ dir' /= opposite dir
      pure res
    f (togo, weight) = let newDir = direction pos togo
                        in (weight, (togo, newDir, if newDir == dir then steps + 1 else 1))
    opposite dir = let Just d' = lookup dir [(North, South), (South, North), (East, West), (West, East)] in d'
    direction from to
      | to - from == V2 1 0 = East
      | to - from == V2 (-1) 0 = West
      | to - from == V2 0 1 = South
      | to - from == V2 0 (-1) = North


main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day17.in"
  print $ solve input 0 3
  print $ solve input 4 10

parseInput :: String -> Map Point [(Point, Int)]
parseInput = mkGraph . parseAsciiMap (Just . read . pure)
  where
    mkGraph mp = flip Map.mapWithKey mp $ \k _ -> 
      map (\t -> (t, mp Map.! t)) (filter (`Map.member` mp) $ ordinalNeighbours k)

-- 1238
-- 1362
