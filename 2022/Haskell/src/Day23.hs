module Day23 where

import Lib (parseAsciiMap, firstRepeatOn, Point, neighbours, safeSucc, freqs, findBounds)
import Linear (V2(..))
import Advent.Coord (up, down, left, right)
import Data.Maybe (mapMaybe, listToMaybe)
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Elves = Set Point

data Direction = North | South | West | East 
  deriving (Eq, Ord, Show, Bounded, Enum)

singleMove :: Direction -> Point
singleMove North = down
singleMove South = up
singleMove West  = left
singleMove East  = right

considerMove :: Direction -> [Point]
considerMove North = [V2 (-1) (-1), V2 0 (-1), V2 1 (-1)]
considerMove South = [V2 (-1) 1, V2 0 1, V2 1 1]
considerMove West  = [V2 (-1) 1, V2 (-1) 0, V2 (-1) (-1)]
considerMove East  = [V2 1 1, V2 1 0, V2 1 (-1)]

allDirections :: Direction -> [(Direction, [Point])]
allDirections = map (\x -> (x, considerMove x)) . take 4 . iterate safeSucc

suggestMove :: Elves -> Direction -> Point -> (Point, Maybe Point)
suggestMove mp dir pos = (pos,) . listToMaybe $ do
  guard $ any (`Set.member` mp) (neighbours pos) -- Should we move at all?
  (toDir, to) <- allDirections dir
  guard $ all ((`Set.notMember` mp) . (pos +)) to
  pure (pos + singleMove toDir)

step :: Elves -> Direction -> Elves
step elves dir = let moves = map (suggestMove elves dir) (Set.toList elves)
                     allSuggestions = freqs (mapMaybe snd moves)
                  in Set.fromList $ map (change elves allSuggestions) moves

change :: Elves -> Map Point Int -> (Point, Maybe Point) -> Point
change mp freq (from, Nothing) = from
change mp freq (from, Just to)
  | to `Map.notMember` freq = to
  | freq Map.! to == 1 = to
  | otherwise = from

score :: Elves -> Int
score elves = length $ do
  x <- [minx..maxx]
  y <- [miny..maxy]
  guard $ V2 x y `Set.notMember` elves
  pure 1
  where
    (minx, miny, maxx, maxy) = findBounds elves

main :: IO ()
main = do
  input <- Map.keysSet . parseAsciiMap (guard . (== '#')) <$> readFile "../data/day23.in"
  let res = scanl step input (iterate safeSucc North)
  print $ score (res !! 10)
  print . fmap fst . firstRepeatOn snd $ zip [0..] res
  
-- 3766
-- 954
