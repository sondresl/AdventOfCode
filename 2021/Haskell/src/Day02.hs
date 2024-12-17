{-# LANGUAGE ScopedTypeVariables #-}

module Day02 where

import Linear
import Control.Lens

run :: forall t. (R2 t, Applicative t, Num (t Int)) => [t Int -> t Int] -> Int
run = productOf (_xy . each) . foldl (&) (pure 0)

part1 :: [(String, Int)] -> Int
part1 = run . map f 
  where
    f ("up", v) = (+ V2 0 (-v))
    f ("down", v) = (+ V2 0 v)
    f ("forward", v) = (+ V2 v 0)

part2 :: [(String, Int)] -> Int
part2 = run . map f
  where
    f ("up", v) = (+ V3 0 0 (-v))
    f ("down", v) = (+ V3 0 0 v)
    f ("forward", v) = \(V3 x y z) -> V3 (x + v) (y + (v * z)) z

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day02.in"

  print $ part1 input
  print $ part2 input

-- 2039256
-- 1856459736

parseInput :: String -> [(String, Int)]
parseInput = map (f . words) . lines
  where
    f [x,y] = (x, read y)
