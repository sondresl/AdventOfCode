module Day07 where

import Lib (commaNums)

align :: (Int -> Int) -> [Int] -> Int
align f input = minimum $ map diff [minimum input .. maximum input]  
  where
    diff x = sum $ map (f . abs . subtract x) input

main :: IO ()
main = do
  input <- commaNums <$> readFile "../data/day07.in"
  let digitSum x = x * (x + 1) `div` 2
  print $ align id input
  print $ align digitSum input
    
-- 348996
-- 98231647
