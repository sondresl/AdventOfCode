module Day13 where

import Lib (perturbations)
import Data.List.Extra (transpose, splitOn)

solve :: String -> [Int]
solve (lines -> x) = reflect (transpose x) <> ((100 *) <$> reflect x)

reflect :: [String] -> [Int]
reflect ls = filter findMirror [1..length ls - 1]
  where
    findMirror n = and (zipWith (==) l r)
      where 
        l = reverse $ take n ls
        r = take n  $ drop n ls

part2 :: Int -> String -> Int
part2 old = head . concatMap (filter (old /=) . solve) . change

change :: String -> [String]
change = perturbations $ \case
      '.'  -> "#"
      '#'  -> "."
      '\n' -> ""

main :: IO ()
main = do
  input <- splitOn "\n\n" <$> readFile "../data/day13.in"
  let p1 = concatMap solve input
  print $ sum p1
  print $ sum $ zipWith part2 p1 input

-- 27664
-- 33991
