module Day11 where

import Lib (tuple, mannDist, Point, parseAsciiMap, combinations)
import Control.Monad (guard)
import Data.List.Extra (transpose, mapAccumL)
import qualified Data.Map as Map
import Linear (V2(..))

expand :: String -> Int -> [Point]
expand input addRows = map (\(V2 x y) -> V2 (cs x) (rs y)) $ parseInput input
  where
    rs y = let Just y' = lookup y (rows addRows $ lines input) in y + y'
    cs x = let Just x' = lookup x (rows addRows $ transpose $ lines input) in x + x'
    rows inc = zip [0..] . snd . mapAccumL f 0
      where f st row = (if all (== '.') row then st + inc else st, st)
    parseInput = Map.keys . parseAsciiMap f
      where f x = guard (x == '#') *> Just ()

main :: IO ()
main = do
  input <- readFile "../data/day11.in"
  let findSum = sum . map (uncurry mannDist . tuple) . combinations 2
  print . findSum $ expand input 1
  print . findSum $ expand input 999999

-- 10154062
-- 553083047914
