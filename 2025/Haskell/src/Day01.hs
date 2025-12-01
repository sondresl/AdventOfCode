module Day01 where

import Lib (count, (.:))
import Data.List (mapAccumL)

main :: IO ()
main = do
  (totalRotation, input) <- parseInput' <$> readFile "../data/day01.in"
  let rotate = (`mod` 100) .: (+)
      countZeros = count (== 0) . scanl rotate 50
  print $ countZeros input
  let expand n = replicate (abs n) (signum n)
  print $ (+ totalRotation) $ countZeros $ concatMap expand input

parseInput' :: String -> (Int, [Int])
parseInput' = mapAccumL p 0 . lines
  where
    p acc = \case
      'R':num -> let (d, m) = read num `divMod` 100 in (acc + d, m)
      'L':num -> let (d, m) = read num `divMod` 100 in (acc + d, negate m)

-- 1086
-- 6268
