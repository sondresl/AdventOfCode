module Day09 where

import Lib (allNums, takeUntil)

solve :: ([Int] -> Int -> Int) -> [Int] -> Int
solve f = foldr f 0 . takeUntil (all (== 0)) . iterate (map (uncurry subtract) . (zip <*> tail))

main :: IO ()
main = do
  input <- map allNums . lines <$> readFile "../data/day09.in"
  print $ sum $ map (solve ((+) . last)) input
  print $ sum $ map (solve ((-) . head)) input

-- 1939607039
-- 1041

