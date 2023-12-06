module Day06 where

import Lib (allNums, binaryMinSearch, tuple)
import Data.Tuple.Extra (both)

run :: (Int, Int) -> Int
run (t, rec) = ub - lb
  where
    beat n = n * (t - n) > rec
    Just lb = binaryMinSearch beat 1 t
    Just ub = binaryMinSearch (not . beat) lb t

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day06.in"
  print . product $ map run input
  print . run . both (read . concatMap show) $ unzip input
    
parseInput :: String -> [(Int, Int)]
parseInput = uncurry zip . tuple . map allNums . lines

-- 800280
-- 45128024
