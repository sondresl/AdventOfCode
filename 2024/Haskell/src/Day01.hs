module Day01 where

import Lib (freqs, allNums, (.:))
import Data.List (transpose, sort)
import qualified Data.Map as Map

part1 :: [Int] -> [Int] -> Int
part1 = sum .: zipWith (abs .: subtract)

part2 :: [Int] -> [Int] -> Int
part2 l (freqs -> r) = foldr ((+) . similar) 0 l
  where
    similar x = x * Map.findWithDefault 0 x r

main :: IO ()
main = do
  [left, right] <- map sort . transpose . map allNums . lines <$> readFile "../data/day01.in"
  print $ part1 left right
  print $ part2 left right
