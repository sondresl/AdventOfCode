module Day07 where

import Lib (allNums)
import Data.Maybe (mapMaybe, listToMaybe)
import Control.Monad (guard)
import Data.List (partition)

type Op = Int -> Int -> Int

combine :: [Op] -> [Int] -> Bool
combine ops (num:x:xs) = not . null $ go x xs
  where
    go total [] = guard (total == num) *> pure num
    go total (x:xs) = do
      op <- ops
      let new = total `op` x
      guard $ new <= num
      go new xs

main :: IO ()
main = do
  input <- map allNums . lines <$> readFile "../data/day07.in"

  let (p1, rest) = partition (combine [(*),(+)]) input 
  print . sum . map head $ p1

  let f a b = a * 10 ^ (floor (logBase 10 (fromIntegral b)) + 1) + b
  print $ sum . map head . (p1 <>) $ filter (combine [(*),(+),f]) rest

-- 2437272016585
-- 162987117690649
