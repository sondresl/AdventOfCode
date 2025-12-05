module Day04 where

import Lib (parseAsciiMap, neighbours, count)
import Data.List (unfoldr)
import Advent.Coord (Coord(..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

removeRolls :: Set Coord -> Maybe (Int, Set Coord)
removeRolls input =
  let removed = Set.filter ((< 4) . count (`Set.member` input) . neighbours) input
   in case Set.size removed of
        0 -> Nothing
        n -> Just (n, input Set.\\ removed)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day04.in"
  let removed = unfoldr removeRolls input
  print $ head removed
  print $ sum removed

parseInput :: String -> Set Coord
parseInput = Map.keysSet . parseAsciiMap f
  where
    f '@' = Just ()
    f _ = Nothing

-- 1346
-- 8493
