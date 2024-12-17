module Day09 where

import Lib (mannDist, ordNub)
import Linear hiding (trace)
import Advent.Coord (origin, right, left, up, down, Coord)
import Data.Set (Set)
import qualified Data.Set as Set

tailPos :: [Coord] -> [Coord]
tailPos xs = origin : go (Set.singleton origin) origin xs
  where
    truncPoint (V2 x y) = V2 (trunc x) (trunc y)
    trunc x = if x > 0 then 1 else if x < 0 then (-1) else 0
    go been p@(V2 px py) [] = []
    go been p@(V2 px py) (x@(V2 xx xy):xs)
      | px /= xx && py /= xy && mannDist x p > 2 || 
       (px == xx || py == xy) && mannDist x p == 2 = 
        let new = p + truncPoint (x - p)
         in new : go (Set.insert new been) new xs
      | otherwise = go been p xs

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day09.in"
  let headPos = scanl (+) origin input
  print $ length $ ordNub $ tailPos headPos
  print . length . ordNub . (!! 9) $ iterate tailPos headPos

parseInput :: String -> [Coord]
parseInput = concatMap (f . words) . lines
  where
    f ["R", read -> n] = replicate n right
    f ["L", read -> n] = replicate n left 
    f ["U", read -> n] = replicate n up 
    f ["D", read -> n] = replicate n down 
    f e = error (show e)

-- 6197
-- 2562
