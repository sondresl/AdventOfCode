module Day11 where

import Lib (iterateN, allNums)
import Data.Digits (digits, unDigits)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

blinkStones :: IntMap Int -> IntMap Int
blinkStones stones = IntMap.fromListWith (+) $ do
  (stone, count) <- IntMap.assocs stones
  map (,count) $ blink stone

blink :: Int -> [Int]
blink 0 = [1]
blink n 
  | odd digs  = [n * 2024]
  | otherwise = [left, right]
    where
      digs = floor (logBase 10 (fromIntegral n)) + 1
      left  = n `div` 10^(digs `div` 2)
      right = n `mod` 10^(digs `div` 2)

main :: IO ()
main = do
  input <- IntMap.fromList . map (,1) . allNums <$> readFile "../data/day11.in"
  print $ sum $ iterateN 25 blinkStones input
  print $ sum $ iterateN 75 blinkStones input

-- 199946
-- 237994815702032
