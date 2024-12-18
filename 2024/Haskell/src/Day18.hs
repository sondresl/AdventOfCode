module Day18 where

import Lib
import Advent.Coord
import Advent.Search
import Data.Maybe
import Data.Either
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V2(V2))

shortestPath :: [Coord] -> Int -> Int -> Maybe Int
shortestPath input bound n = listToMaybe [ l | (l, pos) <- search go [origin]
                                             , pos == V2 bound bound ]
  where
    blocks = Set.fromList $ take n input
    inside (V2 x y) = x <= bound && x >= 0 && y <= bound && y >= 0
    go pos = map (1,) . filter inside . filter (`Set.notMember` blocks) $ ordinalNeighbours pos

main :: IO ()
main = do
  input <- map (uncurry V2 . tuple . allNums) . lines <$> readFile "../data/day18.in"
  print . fromJust $ shortestPath input 70 1024
  let Just firstFailure = binaryMinSearch (isNothing . shortestPath input 70) 1025 (length input)
  putStrLn . (\(V2 x y) -> show x <> "," <> show y) $ input !! (firstFailure - 1)

-- 344
-- 46,18
