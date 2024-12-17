module Day11 where

import Lib (iterateN, allNums)
import Data.Digits (digits, unDigits)
import Data.Map (Map)
import qualified Data.Map as Map

blinkStones :: Map Int Int -> Map Int Int
blinkStones stones = Map.fromListWith (+) $ do
  (stone, count) <- Map.assocs stones
  map (,count) $ blink stone

blink :: Int -> [Int]
blink 0 = [1]
blink n 
  | odd (length digs) = [n * 2024]
  | otherwise         = [left, right]
    where
      digs = digits 10 n
      left = unDigits 10 $ take (length digs `div` 2) digs
      right = unDigits 10 $ drop (length digs `div` 2) digs

main :: IO ()
main = do
  input <- Map.fromList . map (,1) . allNums <$> readFile "../data/day11.in"
  print $ sum $ iterateN 25 blinkStones input
  print $ sum $ iterateN 75 blinkStones input

-- 199946
-- 237994815702032
