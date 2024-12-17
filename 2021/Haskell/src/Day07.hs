module Day07 where

import Lib (commaNums)
import Data.List (sort)

main :: IO ()
main = do
  input <- sort . commaNums <$> readFile "../data/day07.in"
  let run f val = sum . map (f . abs .  subtract val) $ input
      digitSum x = x * (x + 1) `div` 2
      median = input !! (length input `div` 2)
      mean = map ((+) $ sum input `div` length input) [0, 1]
  print $ run id median
  print $ minimum $ map (run digitSum) mean
    
-- 348996
-- 98231647
