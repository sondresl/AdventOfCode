module Day01 where

import Lib (count, linedNums)

main :: IO ()
main = do
  input <- linedNums <$> readFile "../data/day01.in"

  let solve f = count (uncurry (<)) . (zip <*> f)

  print $ solve tail input
  print $ solve (drop 3) input
