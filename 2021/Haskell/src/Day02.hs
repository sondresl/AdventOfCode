module Day02 where

import Lib (tuple)
import Data.List (foldl')
import Linear (V2(..), V3(..), R2, _xy)
import Control.Lens (productOf, each, (&))

run :: (R2 t, Applicative t, Num (t Int)) => [t Int -> t Int] -> Int
run = productOf (_xy . each) . foldl' (&) (pure 0)

part1 :: (String, Int) -> V2 Int -> V2 Int
part1 (str, v) = 
  case str of
    "up" -> (+ V2 0 (-v))
    "down" -> (+ V2 0 v)
    "forward" -> (+ V2 v 0)

part2 :: (String, Int) -> V3 Int -> V3 Int
part2 (str, v) = 
  case str of 
    "up" -> (+ V3 0 0 (-v))
    "down" -> (+ V3 0 0 v)
    "forward" -> \(V3 x y z) -> V3 (x + v) (y + v * z) z

main :: IO ()
main = do
  input <- map (fmap read . tuple . words) . lines <$> readFile "../data/day02.in"

  print . run $ map part1 input
  print . run $ map part2 input

-- 2039256
-- 1856459736
