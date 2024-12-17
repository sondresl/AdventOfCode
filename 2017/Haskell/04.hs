module Main where

import Data.List (sort, tails, nub)
import Data.Function (on)

part1 :: [[String]] -> Int
part1 = length . filter (((==) `on` length) <*> nub)

part2 :: [[String]] -> Int
part2 = length . filter (not . any (uncurry ((==) `on` sort)) . pairs)
  where pairs xs = [ (t, c) | t:ts <- tails xs, c <- ts ]

main :: IO ()
main = do
  input <- map words . lines <$> readFile "../data/04.in"
  print $ part1 input
  print $ part2 input
