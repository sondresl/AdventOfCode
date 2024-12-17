{-# LANGUAGE ViewPatterns #-}
module Day02 where

import Lib (tuple)
import Data.List (foldl')
import Linear (V2(..), V3(..), R2, _xy, _xz)
import Control.Lens (productOf, each, (&), Lens')

run :: (R2 t, Applicative t, Num (t Int)) => Lens' (t Int) (V2 Int) -> [t Int -> t Int] -> Int
run f = productOf (f . each) . foldl' (&) (pure 0)

encode :: (String, String) -> V3 Int -> V3 Int
encode (str, read -> v) = 
  case str of 
    "up" -> (+ V3 0 0 (-v))
    "down" -> (+ V3 0 0 v)
    "forward" -> \(V3 x y z) -> V3 (x + v) (y + v * z) z

main :: IO ()
main = do
  input <- map (encode . tuple . words) . lines <$> readFile "../data/day02.in"

  print . run _xz $ input
  print . run _xy $ input

-- 2039256
-- 1856459736
