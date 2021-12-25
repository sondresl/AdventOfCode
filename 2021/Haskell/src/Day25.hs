module Day25 where

import Lib (count, Point, findBounds, parseAsciiMap, zipWithTail)
import Linear (V2(..))
import Data.Map (Map)
import qualified Data.Map as Map

type Oceanfloor = Map Point Cucumber
data Cucumber = South | East | Space
  deriving (Show, Eq, Ord)

step :: Oceanfloor -> Oceanfloor
step mp = go South south (go East east mp)
  where
    (_,_,mx,my) = findBounds (Map.keys mp)
    south (V2 x y) = V2 x ((y + 1) `mod` (my + 1))
    east (V2 x y) = V2 ((x + 1) `mod` (1 + mx)) y
    go dir add mp = foldr (uncurry Map.insert) mp $ concatMap (\k -> [(k, Space), (add k, dir)]) 
                  $ Map.keys $ Map.filterWithKey (\k v -> v == dir && (mp Map.! add k == Space)) mp

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day25.in"
  print $ (+1) . length . takeWhile (uncurry (/=)) . zipWithTail $ iterate step input

parseInput :: String -> Oceanfloor
parseInput = parseAsciiMap f
  where
    f '>' = Just East
    f 'v' = Just South
    f '.' = Just Space

-- 568
