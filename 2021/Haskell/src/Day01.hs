module Day01 where

import Lib

part1 :: [Int] -> Int
part1 = count (uncurry (<)) . (zip <*> tail)

part2 :: [Int] -> Int
part2 = part1 . map sum . slidingWindow 3

main :: IO ()
main = do
  input <- linedNums <$> readFile "../data/day01.in"
  print $ part1 input
  print $ part2 input
