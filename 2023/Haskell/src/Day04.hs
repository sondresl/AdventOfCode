module Day04 where

import Lib (tuple, allNums)
import Data.List.Extra (intersect, splitOn, unfoldr)
import Data.Tuple.Extra (first)

accumulate :: [(Int, Int)] -> Maybe (Int, [(Int, Int)])
accumulate [] = Nothing
accumulate ((c, wins):xs) = Just (c, map (first (+ c)) (take wins xs) <> drop wins xs)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day04.in"
  let winCount = length . uncurry intersect
  print . sum . map ((2^) . subtract 1) . filter (>0) . map winCount $ input
  print . sum . unfoldr accumulate . map ((1,) . winCount) $ input

parseInput :: String -> [([Int], [Int])]
parseInput = map (first tail . tuple . map allNums . splitOn "|") . lines

-- 26914
-- 13080971
