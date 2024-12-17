module Day07 where

import Lib (allNums)
import Data.Maybe (mapMaybe, listToMaybe)
import Control.Monad (guard)

type Op = Int -> Int -> Int

combine :: [Op] -> [Int] -> Maybe Int
combine ops (num:x:xs) = listToMaybe $ go x xs
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

  print $ sum $ mapMaybe (combine [(*),(+)]) input

  let f x y = read (show x <> show y)
  print $ sum $ mapMaybe (combine [(*),(+),f]) input

-- 2437272016585
-- 162987117690649
