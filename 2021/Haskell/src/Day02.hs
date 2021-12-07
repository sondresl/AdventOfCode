{-# LANGUAGE ViewPatterns #-}
module Day02 where

import Lib (tuple, intoEndo)
import Data.Monoid (Endo(..), Dual(..), appEndo)
import Linear (V3(..), _xy, _xz)
import Control.Lens (productOf, each)

encode :: (String, String) -> V3 Int -> V3 Int
encode (str, read -> v) =
  case str of 
    "up" -> (+ V3 0 0 (-v))
    "down" -> (+ V3 0 0 v)
    "forward" -> \(V3 x y z) -> V3 (x + v) (y + v * z) z

main :: IO ()
main = do
  input <- intoEndo (encode . tuple . words) . lines <$> readFile "../data/day02.in"
  let run f = productOf (f . each) . (`appEndo` V3 0 0 0)
  print $ run _xz input
  print $ run _xy input

-- 2039256
-- 1856459736
