module Day01 where

import Data.List.Extra (splitOn, sortOn)
import Data.Ord (Down(Down))

main :: IO ()
main = do
  input <- sortOn Down . map (sum . map read . lines) . splitOn "\n\n" <$> readFile "../data/day01.in"
  print $ head input
  print . sum . take 3 $ input

-- 69206
-- 197400
