module Day02 where

import Lib (select, allNums, zipWithTail, count)

safe :: [Int] -> Bool
safe xs = (all (> 0) diffs || all (< 0) diffs) && all ((<= 3) . abs) diffs
  where 
    diffs = zipWith subtract xs (tail xs)

main :: IO ()
main = do
  input <- map allNums . lines <$> readFile "../data/day02.in"
  print $ count safe input
  print $ count (any (safe . snd) . select) input

-- 486
-- 540
