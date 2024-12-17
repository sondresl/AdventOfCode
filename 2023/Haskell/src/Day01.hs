module Day01 where

import Lib (lookupJust)
import Data.Maybe (mapMaybe)
import Data.List.Extra (isPrefixOf, find, tails)

ints, numbers :: [String]
ints = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] <> ints

convert :: [(String, Int)]
convert = zip numbers (cycle [1..9])

solve :: [String] -> String -> Int
solve comps input = head opts * 10 + last opts
  where opts = mapMaybe findNumber $ tails input
        findNumber str = (`lookupJust` convert) <$> find (`isPrefixOf` str) comps

main :: IO ()
main = do
  input <- lines <$> readFile "../data/day01.in"
  let run f = sum $ map (solve f) input
  print $ run ints
  print $ run numbers

-- 55477
-- 54431
