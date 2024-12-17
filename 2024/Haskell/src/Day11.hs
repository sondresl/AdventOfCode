module Day11 where

import Lib (iterateN, allNums)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

blinkStones :: IntMap Int -> IntMap Int
blinkStones stones = IM.fromListWith (+) $ do
  (stone, count) <- IM.assocs stones
  (,count) <$> blink stone

blink :: Int -> [Int]
blink 0 = [1]
blink n 
  | odd digs  = [n * 2024]
  | otherwise = [left, right]
    where
      digs = floor (logBase 10 (fromIntegral n)) + 1
      (left, right)  = n `divMod` (10^(digs `div` 2))

main :: IO ()
main = do
  input <- IM.fromListWith (+) . map (,1) . allNums <$> readFile "../data/day11.in"
  print $ sum $ iterateN 25 blinkStones input
  print $ sum $ iterateN 75 blinkStones input

-- 199946
-- 237994815702032
