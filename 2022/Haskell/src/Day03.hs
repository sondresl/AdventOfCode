module Day03 where

import Data.List.Extra (intersect, nub, chunksOf)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  input <- lines <$> readFile "../data/day03.in"

  let halve str = [a,b] where (a,b) = splitAt (length str `div` 2) str
      scoreCommon = sum . map priority . nub . foldl1 intersect
      priority = fromJust . (`lookup` (zip (['a'..'z'] <> ['A'..'Z']) [1..]))

  print . sum . map scoreCommon . map halve  $ input
  print . sum . map scoreCommon . chunksOf 3 $ input
    
-- 8349
-- 2681
