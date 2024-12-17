module Day06 where

import Lib (allNums, binaryMinSearch, count, tuple)
import Data.Tuple.Extra (both)
import Data.List.Extra (transpose)

part1 :: [(Int, Int)] -> Int
part1 = product . map go
  where go (t, rec) = count (beat (t, rec)) [1..t]

part2 :: (Int, Int) -> Int
part2 input@(t, rec) = ub - lb
  where
    Just lb = binaryMinSearch (beat input) 1 t
    Just ub = binaryMinSearch (not . beat input) lb t

beat :: (Int, Int) -> Int -> Bool
beat (t, rec) n = n * (t - n) > rec

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day06.in"
  print $ part1 input
  let input2 = both (read . concatMap show) $ unzip input
  print $ part2 input2
    
parseInput :: String -> [(Int, Int)]
parseInput = map tuple . transpose . map allNums . lines

-- 800280
-- 45128024
