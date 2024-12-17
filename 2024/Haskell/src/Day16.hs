module Day16 where

import Lib (parseAsciiMap)
import Advent.Coord (Coord, east, turnLeft, turnRight, turnAround)
import Advent.Search (dijkstra, bfs)
import Control.Lens (view, _1)
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

minCost :: Map (Coord, Coord) Int -> Coord -> Int
minCost mp end = Set.size . Set.fromList . map (view _1) $ bfs starts next
  where
    startCandidates = [ ((pos, dir), cost) | ((pos, dir), cost) <- Map.assocs mp, pos == end ]
    starts = let m = minimum (map snd startCandidates) in map (\((p, d), c) -> (p, d, c)) (filter ((== m) . snd) startCandidates)
    next (p, d, c) = [ (pos, dir, cost) | ((pos, dir), cost) <- Map.assocs mp, pos == p + turnAround d, cost == c - 1 || cost == c - 1001 ]

cost :: Set Coord -> (Coord, Coord) -> [(Int, (Coord, Coord))]
cost mp (pos, dir) = do
  (pos', dir', cost) <- [ (pos + dir, dir, 1)
                        , (pos + turnLeft dir, turnLeft dir, 1001)
                        , (pos + turnRight dir, turnRight dir, 1001) ]
  guard $ pos' `Set.member` mp
  pure (cost, (pos', dir'))

main :: IO ()
main = do
  (start, end, mp) <- parseInput <$> readFile "../data/day16.in"
  let p1 = dijkstra (cost mp) [(start, east)]
  print $ minimum [ cost | ((pos, dir), cost) <- Map.assocs $ p1, pos == end ]
  print $ minCost p1 end

parseInput :: String -> (Coord, Coord, Set Coord)
parseInput input = (start, end, Map.keysSet mp)
  where
    start = head [ p | (p, 'S') <- Map.assocs mp ]
    end = head [ p | (p, 'E') <- Map.assocs mp ]
    mp = flip parseAsciiMap input $ \case
           '.' -> Just '.'
           'S' -> Just 'S'
           'E' -> Just 'E'
           _ -> Nothing

-- 109496
-- 551
