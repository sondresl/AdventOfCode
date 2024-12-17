{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Digits ( digits )
import Data.Semigroup
import Data.List

pairs :: [Int] -> [(Int, Int)]
pairs xs = do
  (x:xs) <- tails xs
  y <- xs
  pure (x, y)

part1 :: [[Int]] -> Int
part1 = getSum . foldMap (Sum . diff . foldMap tup)
  where
    tup x = (Max x, Min x)
    diff (Max x, Min y) = abs (x - y)

part2 :: [[Int]] -> Int
part2 = getSum . foldMap (foldMap tup . pairs)
  where
    tup (x, y) = let (a, b) = (max x y, min x y)
                  in if rem a b == 0
                        then Sum (a `div` b)
                        else Sum 0

main :: IO ()
main = do
  input <- map ((map (read @Int)) . words) . lines <$> readFile "../data/02.in"
  print $ part1 input
  print $ part2 input
