module Day25 where

import Lib (parseAsciiMap, count, (.:))
import Linear (V2(V2))
import Advent.Coord (Coord, origin)
import Control.Monad (guard)
import Data.List.Extra (splitOn, partition)
import qualified Data.Map as Map

heights :: ([Int] -> Int) -> [Coord] -> [Int]
heights comp kl = map f [0..4]
  where f col = comp [ y | V2 x y <- kl, x == col ]

solve :: [[Int]] -> [[Int]] -> Int
solve = count (and . uncurry (zipWith (<))) .: liftA2 (,)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day25.in"
  let (locks, keys) = input
      locks' = map (heights maximum) locks
      keys'  = map (heights minimum) keys
  print $ solve locks' keys'
   
parseInput :: String -> ([[Coord]], [[Coord]])
parseInput = partition (origin `elem`) . map (Map.keys . parseAsciiMap f) . splitOn "\n\n"
  where f c = guard (c == '#') *> Just c

-- 3155
