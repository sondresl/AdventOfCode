module Day03 where

import Control.Monad (guard)

solve :: Int -> [Int] -> Int
solve v = head . run v 0
  where
    run 0 val _ = pure val
    run n val xs = do
      t <- [9,8,7,6,5,4,3,2,1]
      let rest = dropWhile (/= t) xs
      guard $ length rest >= n
      run (n - 1) (val * 10 + head rest) (tail rest)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day03.in"
  print $ sum $ map (solve 2) input
  print $ sum $ map (solve 12) input

parseInput :: String -> [[Int]]
parseInput = map (map (read . pure)) . lines

-- 17554
-- 175053592950232
