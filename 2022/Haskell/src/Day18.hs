module Day18 where

import Lib (neighbours6, count, bfs, dfs, findBounds3)
import Linear hiding (trace)
import Data.Monoid
import Data.Semigroup
import Control.Lens (view)
import Data.List.Extra (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

faces :: Set (V3 Int) -> V3 Int -> Set (V3 Int)
faces ps p = Set.fromList (neighbours6 p) Set.\\ ps

floodFillDroplet :: Set (V3 Int) -> Set (V3 Int)
floodFillDroplet ps = Set.fromList $ bfs [V3 (pred minx) (pred miny) (pred minz)] mkNs id 
  where
    mkNs = filter ((&&) <$> insideBox <*> (`Set.notMember` ps)) . neighbours6
    (minx, miny, minz, maxx, maxy, maxz) = findBounds3 ps
    insideBox (V3 x y z ) = x >= minx - 1 && x <= maxx + 1
                         && y >= miny - 1 && y <= maxy + 1
                         && z >= minz - 1 && z <= maxz + 1

part2 :: Set (V3 Int) -> Int
part2 ps = sum . map (length . common . faces ps) $ Set.toList ps
  where common = Set.intersection (floodFillDroplet ps)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day18.in"
  let air = map (faces input) $ Set.toList input
  print $ sum $ map Set.size air
  print $ part2 input

parseInput :: String -> Set (V3 Int)
parseInput = Set.fromList . map (f . splitOn ",") . lines
  where
    f (map read -> [x,y,z]) = V3 x y z

-- 3530
-- 2000
