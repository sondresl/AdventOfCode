module Day03 where

import Lib (count, binToInt)
import Data.Function (on)
import Data.List.Extra (transpose)
import Data.Tuple.Extra (both, (&&&))

gammaEpsilon :: [String] -> (String, String)
gammaEpsilon = foldMap go . transpose
  where
    go str
      | count (== '1') str >= count (== '0') str = ("1", "0")
      | otherwise = ("0", "1")

keep :: ((String, String) -> String) -> [String] -> String
keep _ [x] = x
keep f strs =
  let val = head . f $ gammaEpsilon strs
    in val : keep f (map tail $ filter ((== val) . head) strs)

run :: ([String] -> (String, String)) -> [String] -> Int
run f = uncurry (*) . both binToInt . f

main :: IO ()
main = do
  input <- lines <$> readFile "../data/day03.in"
  print $ run gammaEpsilon input
  print $ run (keep fst &&& keep snd) input

-- 693486
-- 3379326
