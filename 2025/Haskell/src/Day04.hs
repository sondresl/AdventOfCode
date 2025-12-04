module Day04 where

import Lib (firstRepeat, parseAsciiMap, neighbours, zipWithTail)
import Advent.Coord (Coord(..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

removeRoll :: Set Coord -> Set Coord
removeRoll input = Set.filter f input
  where f = (> 3) . length . filter (`Set.member` input) . neighbours

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day04.in"
  let iterations = scanl (\acc (a, b) ->  acc + a - b) 0 . zipWithTail . map Set.size $ iterate removeRoll input
  print $ iterations !! 1
  print $ firstRepeat iterations

parseInput :: String -> Set Coord
parseInput = Map.keysSet . parseAsciiMap f
  where
    f '@' = Just ()
    f _ = Nothing

-- 1346
-- 8493
