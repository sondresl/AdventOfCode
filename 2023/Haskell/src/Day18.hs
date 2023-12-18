module Day18 where

import Lib (zipWithTail, Point)
import Data.Function (on)
import Data.Char (digitToInt)
import Advent.Coord (down, left, origin, right, up, Dir)
import Linear (V2(V2), Metric(distance))

corners :: [(Dir, Int, String)] -> [Point]
corners = scanl next origin
  where next p (dir, st, _) = p + (dir * V2 st st)

-- Total area + line segments of a simple polygon
-- https://en.wikipedia.org/wiki/Pick%27s_theorem
countInternalPoints :: [Point] -> Int
countInternalPoints input = shoelace input + (outside input `div` 2) + 1 
  where
    outside = sum 
            . map (round . uncurry (distance `on` fmap fromIntegral))
            . zipWithTail
    shoelace = (`div` 2) . abs . sum 
             . map (\(V2 x y, V2 a b) -> (x + a) * (y - b)) 
             . zipWithTail

convertHex :: (Dir, Int, String) -> (Dir, Int, String)
convertHex (_, _, tail -> hex) = (dir, num, "")
  where
    num = foldl (\acc n -> acc * 16 + digitToInt n) 0 $ take 5 hex
    dir = case last hex of
            '0' -> right
            '1' -> up
            '2' -> left
            '3' -> down

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day18.in"
  let solve = countInternalPoints . corners
  print $ solve input
  print . solve $ map convertHex input

parseInput :: String -> [(Dir, Int, String)]
parseInput = map (f . words) . lines
  where 
    f [dir, steps, tail . init -> color] = (direction dir, read @Int steps, color)
    direction = \case
      "R" -> right
      "L" -> left
      "U" -> down
      "D" -> up

-- 95356
-- 92291468914147
