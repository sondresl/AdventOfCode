module Day02 where

import Lib (select, allNums, zipWithTail, count)

part1 = count id . map (safe . map (uncurry subtract) . zipWithTail)
  where
    safe xs = (all (> 0) xs || all (< 0) xs)
            && all (<= 3) (map abs xs)

part2 :: [[Int]] -> Int
part2 input = safeReports + dampened
  where
    safeReports = count (safe . diffs) input
    unsafe = filter (not . safe . diffs) input
    dampened = count (not . null) $ map (filter (safe . diffs) . map snd . select) unsafe
    diffs = map (uncurry subtract) . zipWithTail
    safe xs = (all (> 0) xs || all (< 0) xs)
            && all (<= 3) (map abs xs)

main :: IO ()
main = do
  input <- map allNums . lines <$> readFile "../data/day02.in"
  print $ part1 input
  print $ part2 input

-- 486
-- 540
