module Day09 where

import Lib (allNums, takeUntil)

solve :: [Int] -> Int
solve = foldr ((-) . head) 0 
      . takeUntil (all (== 0)) 
      . iterate (map (uncurry subtract) . (zip <*> tail))

main :: IO ()
main = do
  input <- map allNums . lines <$> readFile "../data/day09.in"
  print $ sum $ map (solve . reverse) input
  print $ sum $ map solve input

-- 1939607039
-- 1041

