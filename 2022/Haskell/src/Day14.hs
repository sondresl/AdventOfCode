module Day14 where

import Lib (firstRepeatOn, tuple, takeUntil)
import Linear (V2(..), _y)
import Advent.Coord (up, left, right)
import Data.Maybe (maybe)
import Control.Lens (view)
import Data.List.Extra (splitOn, find)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (foldM)
import Control.Monad.Identity

type Rock = Set (V2 Int)

allRock :: [[V2 Int]] -> Rock
allRock xs = Set.fromList $ concatMap (concatMap f . (zip <*> tail)) xs
  where
    f (from, to) = takeUntil (== to) $ iterate (+ dir) from
      where dir = signum $ to - from

run :: Monad m => (Rock -> m Rock) -> Int -> Rock -> V2 Int -> m Rock
run f lowestPoint rock p@(V2 x y)
  | y == lowestPoint = f rock
  | p `Set.member` rock = pure rock
  | otherwise = Set.insert p <$> foldM (run f lowestPoint) rock [up + p, left + up + p, right + up + p] 

main :: IO ()
main = do
  input <- allRock . parseInput <$> readFile "../data/day14.in"
  let lowestPoint = 2 + Set.findMax (Set.map (view _y) input)
  let Left p1     = run Left     lowestPoint input (V2 500 0)
  let Identity p2 = run Identity lowestPoint input (V2 500 0)
  print $ Set.size p1 - Set.size input
  print $ Set.size p2 - Set.size input

parseInput :: String -> [[V2 Int]]
parseInput = map (map (uncurry V2 . tuple. map read . splitOn ",") . splitOn " -> ") . lines

-- 763
-- 23921
