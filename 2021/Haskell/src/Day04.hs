module Day04 where

import Lib (takeUntil, commaNums)
import Data.List.Extra (transpose, splitOn, maximumOn, minimumOn, sortOn)
import Data.Tuple.Extra ((&&&), second, both)

type Board = [[Int]]
type Result = (Int, Int)

playAll :: [Int] -> [Board] -> Result
playAll xs = both (uncurry (*)) 
           . (head &&& last) 
           . map (second (xs !!)) 
           . sortOn snd 
           . map ((play xs) . ((<>) <*> transpose))

play :: [Int] -> Board -> (Int, Int) -- Score, turns
play nums board = (score . last &&& subtract 1 . length)
                . tail 
                . takeUntil (any null) 
                $ scanl (flip $ map . filter . (/=)) board nums
  where 
    score = sum . map sum . take 5

main :: IO ()
main = do
  (nums, boards) <- parseInput <$> readFile "../data/day04.in"
  let (part1, part2) = playAll nums boards 
  print part1
  print part2

parseInput :: String -> ([Int], [Board])
parseInput input = (nums, grids)
  where (x:xs) = splitOn "\n\n" input
        nums = commaNums x
        grids = map ((map (map read . words)) . lines) xs

-- 38594
-- 21184
