module Day02 where

import Lib (tuple)

data RPC = Rock | Paper | Scissors
  deriving (Enum, Show, Eq, Ord, Bounded)

win, lose :: (Eq a, Bounded a, Enum a) => a -> a
win a = if a == maxBound then minBound else succ a
lose a = if a == minBound then maxBound else pred a

part1 :: RPC -> RPC -> Int
part1 a b = outcome a b + score b
  where 
    score r = let Just v = lookup r [(Rock, 1), (Paper, 2), (Scissors, 3)] in v
    outcome a b
      | b == win a = 6
      | a == b = 3
      | otherwise = 0

part2 :: RPC -> RPC -> Int
part2 a b = part1 a (action a b)
  where
    action = \case
      Rock -> lose
      Paper -> id
      Scissors -> win

main :: IO ()
main = do
  input <- map (tuple . map parseInput . words) . lines <$> readFile "../data/day02.in"
  let run f = print . sum . map (uncurry f)
  run part1 input
  run part2 input
    where
      parseInput a
        | a `elem` ["X", "A"] = Rock
        | a `elem` ["Y", "B"] = Paper
        | a `elem` ["Z", "C"] = Scissors
        | otherwise = error a

-- 12740
-- 11980
