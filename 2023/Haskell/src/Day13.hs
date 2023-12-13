module Day13 where

import Lib (perturbations)
import Data.List.Extra (transpose, splitOn)

solve :: String -> [Int]
solve (lines -> x) = reflect (transpose x) <> ((100 *) <$> reflect x)

reflect :: [String] -> [Int]
reflect ls = filter findMirror [1..length ls - 1]
  where
    findMirror n = l' == r'
      where 
        l = take n ls
        r = reverse $ take n (drop n ls)
        short = min (length l) (length r)
        (l', r') = (drop (length l - short) l, take short r)

part2 :: Int -> String -> Int
part2 old ls = head $ concatMap (filter (old /=) . solve) (change ls)

change :: String -> [String]
change = perturbations f
    where 
      f '.' = "#"
      f '#' = "."
      f _ = ""

main :: IO ()
main = do
  input <- splitOn "\n\n" <$> readFile "../data/day13.in"
  let p1 = concatMap solve input
  print $ sum p1
  print $ sum $ zipWith part2 p1 input

-- 27664
-- 33991
