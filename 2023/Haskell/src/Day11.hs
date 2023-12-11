module Day11 where

import Lib (mannDist, Point, parseAsciiMap, combinations)
import Control.Monad (guard)
import Data.List.Extra (transpose)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V2(..))

findSum :: String -> Int -> Int
findSum input addRows = sum $ do
  let expanded = Set.toList $ expand input addRows 
  [x, y] <- combinations 2 expanded
  pure $ mannDist x y

expand :: String -> Int -> Set Point
expand input addRows = Set.map (\(V2 x y) -> V2 (cs x) (rs y)) coords
  where
    coords = parseInput input
    rs y = let Just y' = lookup y (rows addRows $ lines input) in y + y'
    cs x = let Just x' = lookup x (rows addRows $ transpose $ lines input) in x + x'

rows :: Int -> [String] -> [(Int, Int)]
rows increase = zip [0..] . go 0
  where
    go n [] = []
    go n (x:xs) = if all (== '.') x then n : go (n + increase) xs else n : go n xs

main :: IO ()
main = do
  input <- readFile "../data/day11.in"
  print $ findSum input 1
  print $ findSum input 999999

parseInput :: String -> Set Point
parseInput = Map.keysSet . parseAsciiMap f
  where f x = guard (x == '#') *> Just ()

-- 10154062
-- 553083047914
