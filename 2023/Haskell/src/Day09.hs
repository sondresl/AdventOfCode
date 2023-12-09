module Day09 where

solve :: [Int] -> Int
solve = foldr ((-) . head) 0 
      . takeWhile (any (/= 0)) 
      . iterate (zipWith subtract <*> tail)

main :: IO ()
main = do
  input <- map (map read . words) . lines <$> readFile "../data/day09.in"
  print $ sum $ map (solve . reverse) input
  print $ sum $ map solve input

-- 1939607039
-- 1041

