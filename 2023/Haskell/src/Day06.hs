module Day06 where

import Lib (allNums, binaryMinSearch, count, tuple)
import Data.Tuple.Extra (both)
import Data.List.Extra (transpose)

run :: (Int, Int) -> Int
run input@(t, rec) = ub - lb
  where
    beat (t, rec) n = n * (t - n) > rec
    Just lb = binaryMinSearch (beat input) 1 t
    Just ub = binaryMinSearch (not . beat input) lb t

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day06.in"
  print . product $ map run input
  print . run . both (read . concatMap show) $ unzip input
    
parseInput :: String -> [(Int, Int)]
parseInput = uncurry zip . tuple . map allNums . lines

-- 800280
-- 45128024
