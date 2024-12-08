module Day07 where

import Lib (allNums)
import Data.List (partition)
import Data.Set (Set)
import qualified Data.Set as Set

combine :: [Int -> Int -> Int] -> [Int] -> Bool
combine ops (num:x:xs) = any (== num) $ foldl go (Set.singleton x) xs
  where
    go acc val = Set.filter (<= num) 
               $ foldMap (\op -> Set.map (\v -> v `op` val) acc) ops

main :: IO ()
main = do
  input <- map allNums . lines <$> readFile "../data/day07.in"

  let (p1, rest) = partition (combine [(*),(+)]) input 
  print . sum . map head $ p1

  let f a b = a * 10 ^ (floor (logBase 10 (fromIntegral b)) + 1) + b
  print . sum . map head . (p1 <>) $ filter (combine [(*),(+),f]) rest

-- 2437272016585
-- 162987117690649
