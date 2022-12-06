module Day06 where

import Lib (slidingWindow, ordNub)
import Data.List.Extra (findIndex)

main :: IO ()
main = do
  input <- init <$> readFile "../data/day06.in"
  let solve n = fmap (+n) . findIndex ((== n) . length . ordNub) . slidingWindow n
  print $ solve 4 input
  print $ solve 14 input

-- Just 1655
-- Just 2665
