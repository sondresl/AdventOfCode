module Day02 where

import Lib (select, allNums, zipWithTail, count)

solve :: ([Int] -> [[Int]]) -> [[Int]] -> Int
solve f = count (any (safe . diffs) . f)
  where
    diffs = zipWith subtract <*> tail
    safe xs = (all (> 0) xs || all (< 0) xs)
            && all ((<= 3) . abs) xs

main :: IO ()
main = do
  input <- map allNums . lines <$> readFile "../data/day02.in"
  print $ solve pure input
  print $ solve (map snd . select) input

-- 486
-- 540
