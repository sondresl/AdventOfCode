module Day09 where

import Lib (mannDist, ordNub)
import Linear (V2(..))
import Advent.Coord (origin, right, left, up, down, Coord)

runTail :: [Coord] -> [Coord]
runTail = scanl go origin
  where
    oneAway x y = abs (x - y) < 2
    go p@(V2 px py) x@(V2 xx xy)
      | oneAway px xx, oneAway py xy = p
      | otherwise = p + signum (x - p)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day09.in"
  let headPos = scanl (+) origin input
      tailPos n = length . ordNub . (!! n) . iterate runTail $ headPos
  print $ tailPos 1
  print $ tailPos 9

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
