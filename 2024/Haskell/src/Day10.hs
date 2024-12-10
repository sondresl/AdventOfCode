module Day10 where

import Lib (parseAsciiMap, ordinalNeighbours)
import Advent.Coord (Coord)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- 'Smart' solution that is just as fast/slow as the dumb dfs with
-- backtracking. This uses dynamic programming to 'cache' the result at every
-- point, and combining these caches at intersections so the final results are
-- in every start node.
general :: Map Coord Int -> Map Coord (Map Coord Int)
general input = flip Map.restrictKeys (Set.fromList starts) $ foldl go ends starts
  where
    ends = Map.unions [ Map.singleton pos (Map.singleton pos 1) | (pos, 9) <- Map.assocs input ]
    starts = [ pos | (pos, 0) <- Map.assocs input ]
    go seen pos = foldr f (Map.insert pos Map.empty seen) next
      where
        val = input Map.! pos
        next = filter ((== Just (val + 1)) . (`Map.lookup` input))
             $ ordinalNeighbours pos
        f new seen = case Map.lookup new seen of
          Just i -> Map.insertWith (Map.unionWith (+)) pos i seen
          Nothing -> let seen' = go seen new
                      in Map.insertWith (Map.unionWith (+)) pos (seen' Map.! new) seen'

main :: IO ()
main = do
  input <- parseAsciiMap (Just . read . pure) <$> readFile "../data/day10.in"
  let result = general input
  print $ sum $ Map.size <$> result
  print $ sum $ sum      <$> result

-- 811
-- 1794
