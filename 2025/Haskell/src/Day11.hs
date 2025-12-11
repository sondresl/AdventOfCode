module Day11 where

import Lib (zipWithTail)
import Control.Lens ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map

pathsFromTo :: Map String [String] -> String -> String -> Int
pathsFromTo input from to = mp Map.! from
  where
    mp = input <&> \edges ->
      if to `elem` edges
        then 1
        else sum $ map (\edge -> Map.findWithDefault 0 edge mp) edges

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day11.in"
  print $ pathsFromTo input "you" "out"
  -- From testing, there is no path from dac to fft, so we must go in this order:
  -- svr -> fft -> dac -> out
  print $ product $ map (uncurry $ pathsFromTo input) $ zipWithTail ["svr", "fft", "dac", "out"]
    
parseInput :: String -> Map String [String]
parseInput = foldMap (p . words) . lines
  where
    p (x:xs) = Map.unionsWith (<>) $ map (Map.singleton (init x)) (map pure xs)

-- 534
-- 499645520864100
