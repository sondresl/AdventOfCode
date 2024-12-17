module Day13 where

import Lib (display, findBounds)
import Linear (V2(..), _x, _y)
import Control.Lens (over, each, Lens', view)
import Data.List.Extra (splitOn, union)

fold :: [V2 Int] -> (String, Int) -> [V2 Int]
fold points (dir, val) =
  let keep = filter (\v2 -> view lens v2 < val) points
      toFold = over (each . lens) (dist val) $ filter (\v2 -> view lens v2 > val) points
   in keep `union` toFold
  where
    dist val x = val - abs (val - x)
    (_, _, mx, my) = findBounds points
    lens :: Lens' (V2 Int) Int
    lens = if dir == "x" then _x else _y

main :: IO ()
main = do
  (coords, folds) <- parseInput <$> readFile "../data/day13.in"
  print . length $ fold coords (head folds)
  let pattern = foldl fold coords folds
  display pattern

parseInput :: String -> ([V2 Int], [(String, Int)])
parseInput = f . splitOn "\n\n"
  where f [x,y] = (coords x, folds y)
        coords = map ((\[x,y] -> (V2 (read x) (read y))) . splitOn ",") . lines
        folds = concat . map (map ((\[x,y] -> (x, read y)) . splitOn "=") . drop 2 . words) . lines

-- 810
-- HLBUBGFR
