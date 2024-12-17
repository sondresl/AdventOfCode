module Day04 where

import Lib (count)
import Data.List.Extra (splitOn)
import Data.IntSet (IntSet, isSubsetOf, disjoint, fromList)

main :: IO ()
main = do
  input <- map parseInput . lines <$> readFile "../data/day04.in"
  print $ count ((||) <$> uncurry isSubsetOf <*> uncurry (flip isSubsetOf)) input
  print $ count (not . uncurry disjoint) input

parseInput :: String -> (IntSet, IntSet)
parseInput str = (fromList [a..b], fromList [c..d])
  where
    [[a,b], [c,d]] = map (map read . splitOn "-") . splitOn "," $ str

-- 580
-- 895
