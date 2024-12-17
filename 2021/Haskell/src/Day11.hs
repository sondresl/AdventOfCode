module Day11 where

import Lib (iterateMaybe, count, parseAsciiMap, neighbours, Point, takeUntil)
import Data.Maybe (listToMaybe)
import Data.Map (Map)
import qualified Data.Map as Map

type Coordinates = Map Point Int

flash :: Coordinates -> Maybe Coordinates
flash input = do
  (k, v) <- listToMaybe $ Map.toList $ Map.filter (> 9) input
  let f x = if x == 0 then 0 else x + 1
      new = foldr (Map.adjust f) input 
          . filter (`Map.member` input) 
          $ neighbours k
  pure $ Map.insert k 0 new

main :: IO ()
main = do
  input <- parseAsciiMap (Just . read . pure) <$> readFile "../data/day11.in"
  let run = map (count (== 0)) 
          . tail 
          . iterate (last . iterateMaybe flash . Map.map (+1))
          $ input

  print . sum $ take 100 run
  print . length $ takeUntil (== 100) run

-- 1655
-- 337
