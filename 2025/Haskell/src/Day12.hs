module Day12 where

import Lib (parseAsciiMap, allNums, count)
import Advent.Coord (Coord)
import Data.List.Extra (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V2(..))

data Region = Region
  { bounds :: Coord
  , blocks :: Map Int Int
  } deriving Show

fitsInArea presents (Region (V2 x y) blocks) = total < x * y
  where
    areas = Map.fromList $ zip [0..] $ map Set.size presents
    total = sum $ Map.unionWith (*) areas blocks

main :: IO ()
main = do
  (presents, regions) <- parseInput <$> readFile "../data/day12.in"
  print $ count (fitsInArea presents) regions

parseInput :: String -> ([Set Coord], [Region])
parseInput input = (shapes, regions)
  where
    ls = splitOn "\n\n" input
    regions = map (f . allNums) . lines $ last ls
    f (x:y:rest) = Region (V2 x y) (Map.fromList $ zip [0..] rest)
    shapes = map p $ init ls
    p = Map.keysSet . parseAsciiMap g . unlines . tail . lines
    g = \case
      '#' -> Just ()
      '.' -> Nothing

-- 469
