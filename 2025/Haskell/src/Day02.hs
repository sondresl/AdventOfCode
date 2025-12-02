module Day02 where

import Lib (tuple)
import Data.List.Extra (splitOn, chunksOf)

same :: Eq a => [a] -> Bool
same [] = True
same (x:xs) = all (== x) xs

part1 :: String -> Bool
part1 str = even (length str) && l == r
  where
    mx = length str `div` 2
    (l, r) = (take mx str, drop mx str)

part2 :: String -> Bool
part2 str = any same cs
  where
    mx = length str `div` 2
    divs = filter (\i -> (length str `mod` i) == 0) [1..mx]
    cs = map (`chunksOf` str) divs

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day02.in"
  let nums = concatMap (map show . (\(x,y) -> [x..y])) input
      run f = sum $ map read $ filter f nums
  print $ run part1
  print $ run part2

parseInput :: String -> [(Int, Int)]
parseInput = map (tuple . map (read @Int) . splitOn "-") . splitOn ","

-- 29818212493
-- 37432260594
