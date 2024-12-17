{-# LANGUAGE ViewPatterns #-}
module Day03 where

import Control.Lens
import Data.Maybe (fromJust)
import Linear (V2 (..))
import Lib ( count )
import Data.List (sort)
import Data.List.Extra

import Debug.Trace

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

type Triple = (Int, Int, Int)

part1 :: [[Int]] -> Int
part1 = count valid
  where
    valid (sort -> [a, b, c]) = a + b > c

part2 :: [[Int]] -> Int
part2 = part1 . concatMap transpose . chunksOf 3

main :: IO ()
main = do
  let run file = do
        input <- parseInput <$> readFile file
        putStrLn ("\nInput file: " ++ show file)
        print $ part1 input
        print $ part2 input

  run "../data/day03.in"

-- 1032
-- 1838
