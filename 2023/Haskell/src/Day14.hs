module Day14 where

import Lib (iterateN, skipLoop)
import Data.List.Extra (intercalate, sortOn, transpose, splitOn)
import Data.Ord (Down(Down))

roll :: [String] -> [String]
roll = map (intercalate "#" . map (sortOn Down) . splitOn "#")

part2 :: [String] -> Int
part2 = score . skipLoop (0,) (const $ const id) 1000000000 . iterate oneCycle
  where oneCycle = iterateN 4 (transpose . map reverse . roll)

score :: [String] -> Int
score = sum . concatMap (f . reverse)
  where f line = [ v | (v,'O') <- zip [1..] line]

main :: IO ()
main = do
  input <- transpose . lines <$> readFile "../data/day14.in"
  print $ score $ roll input
  print $ part2 input

-- 112773
-- 98894
