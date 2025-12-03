module Day03 where

import Lib (select)
import Data.List.Extra (maximumOn)

toDecimal :: [Int] -> Int
toDecimal = foldl ((+) . (* 10)) 0

solve :: Int -> [Int] -> Int
solve n xs = go (reverse start) end
  where
    (start, end) = splitAt (length xs - n) xs
    go [] en = toDecimal en
    go (s:st) en = go st next
      where
        next = maximumOn toDecimal $ (en:) $ map ((s:) . snd) (select en)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day03.in"
  print $ sum $ map (solve 2) input
  print $ sum $ map (solve 12) input

parseInput :: String -> [[Int]]
parseInput = map (map (read . pure)) . lines

-- 17554
-- 175053592950232
