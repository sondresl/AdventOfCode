module Day06 where

import Data.List.Extra (transpose, splitOn)

solve :: (String, [Int]) -> Int
solve (op, nums) = case op of
  "*" -> product nums
  "+" -> sum nums

main :: IO ()
main = do
  input <- readFile "../data/day06.in"
  print . sum . map solve $ parseNums input
  print . sum . map solve $ parseTransposed input

parseNums :: String -> [(String, [Int])]
parseNums input = map f . transpose . map words $ lines input
  where
    f xs = let ns = (init xs)
            in (last xs, map read ns)

parseTransposed :: String -> [(String, [Int])]
parseTransposed input = zip ops (map (map read) nums)
  where
    ops = words (last (lines input))
    nums = splitOn [""] . map (filter (/= ' ')) $ transpose $ init $ lines input

-- 4583860641327
-- 11602774058280
