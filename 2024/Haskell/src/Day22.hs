module Day22 where

import Lib (maximumVal', iterateN, allNums, slidingWindow)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Bits (xor)

generateSecret :: Int -> Int
generateSecret = gen (* 2048) . gen (`div` 32) . gen (* 64)
  where gen f n = (f n `xor` n) `mod` 16777216

changes :: Int -> Map [Int] Int
changes input = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty
              . (`zip` drop 4 xs) 
              . slidingWindow 4 
              $ zipWith subtract xs (tail xs)
  where
    xs = map (`mod` 10) . take 2000 $ iterate generateSecret input

main :: IO ()
main = do
  input <- allNums <$> readFile "../data/day22.in"
  print $ sum (map (iterateN 2000 generateSecret) input)
  let p2 = snd . maximumVal' . foldr (Map.unionWith (+) . changes) Map.empty
  print $ p2 input

-- 19150344884
-- 2121
