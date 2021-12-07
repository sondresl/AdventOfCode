module Day06 where

import Lib
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map

step :: Map Int Int -> Map Int Int
step input = Map.insert 8 negs 
           . Map.insertWith (+) 6 negs 
           $ Map.insert (-1) 0 input'
  where 
    input' = Map.foldMapWithKey (Map.singleton . subtract 1) input
    negs = Map.findWithDefault 0 (-1) input'

main :: IO ()
main = do
  input <- freqs . sort . commaNums <$> readFile "../data/day06.in"
  let run n = sum . iterateN n step
  print $ run 80 input
  print $ run 256 input

-- 350917
-- 1592918715629
