module Day06 where

import Lib (firstRepeat, iterateMaybe, parseAsciiMap)
import Advent.Coord (down, turnLeft, Coord, Dir)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Tiles = Floor | Block | Start
  deriving (Show, Eq, Ord)

type Lab = Map Coord Tiles
type Guard = (Coord, Dir)

step :: Lab -> Guard -> Maybe Guard
step lab (pos, dir) = 
    case Map.lookup (pos + dir) lab of
      Nothing -> Nothing
      Just Floor -> Just (pos + dir, dir)
      Just Block -> Just (pos, turnLeft dir)

tryLoop :: Lab -> Guard -> Maybe Coord
tryLoop lab (pos, dir) = firstRepeat path *> Just (pos + dir)
  where 
    lab' = Map.insert (pos + dir) Block lab
    path = iterateMaybe (step lab') (pos, dir)

-- Filter out any (position, facing) that would cause a block to be placed in
-- the path taken to get here, as that would make it impossible to get to the
-- current position in the first place. Optimization before trying to find
-- every loop.
inPath :: [Guard] -> [Guard]
inPath = go Set.empty
  where
    go seen [] = []
    go seen (p@(pos, dir):rest)
      | Set.member (pos + dir) seen = go (Set.insert pos seen) rest
      | otherwise = p : go (Set.insert pos seen) rest

main :: IO ()
main = do
  (lab, guard) <- parseInput <$> readFile "../data/day06.in"

  let path = iterateMaybe (step lab) guard
  print . Set.size . Set.fromList . map fst $ path

  let floorInfront (pos, dir) = Map.lookup (pos + dir) lab == Just Floor
      unblocked = inPath $ filter floorInfront path
  print . Set.size . Set.fromList $ mapMaybe (tryLoop lab) unblocked

parseInput :: String -> (Lab, Guard)
parseInput input = (Map.insert start Floor lab, (start, down))
  where
    lab = flip parseAsciiMap input $ \case
      '#' -> Just Block
      '.' -> Just Floor
      '^' -> Just Start
    start = head [ pos | (pos, Start) <- Map.assocs lab ]

-- 4789
-- 1304
