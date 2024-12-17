{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Digits ( digits )
import Data.Monoid
import Data.Coerce
import qualified Data.Vector as V

import Debug.Trace

part1 :: [Int] -> Int
part1 = coerce . foldMap (Sum . fst) . filter (uncurry (==)) . (zip <*> tail) . ((:) =<< last)

part2 :: [Int] -> Int
part2 (V.fromList -> xs) = getSum $ foldMap test [0..(len - 1)]
  where
    len = V.length xs `div` 2
    test x = if xs V.! x == xs V.! (x + len)
                then Sum (2 * (xs V.! x))
                else Sum 0

main :: IO ()
main = do
  input <- map (read @Int . pure) . init <$> readFile "../data/01.in"
  print $ part1 input
  print $ part2 input
