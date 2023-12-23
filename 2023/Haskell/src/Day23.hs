module Day23 where

import Lib
import Advent.Coord
import Advent.Search hiding (dfs)
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Text.RawString.QQ
import Text.ParserCombinators.Parsec hiding (count)
import Linear hiding (trace)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Tuple.Extra (first)
import Debug.Trace

part1 input = bfsOn (\(x,y,z) -> (x,y)) [(0, V2 1 0, V2 1 (-1))] nexts
  where
    nexts (i, pos, prev) = map (i+1,,pos)
                         . filter (/= prev)
                         . filter (notSlope pos) 
                         . filter (`Map.member` input)
                         $ ordinalNeighbours pos
    notSlope pos c = case (c - pos, input Map.! c) of
      (V2 0 1   , '^') -> False
      (V2 1 0   , '<') -> False
      (V2 0 (-1), 'v') -> False
      (V2 (-1) 0, '>') -> False
      _ -> True

mkGraph :: Map Point Char -> Map Point [(Int, Point)]
mkGraph input = foldMap steps crossings
  where
    (minx,miny,maxx,maxy) = findBounds (Map.keys input)
    crossings = ([V2 1 0,  V2 maxx maxy] <>) . filter ((>2) . length . filter (`Map.member` input) . ordinalNeighbours) 
              $ Map.keys input
    steps pos = Map.singleton pos . filter ((`elem` crossings) . snd) . tail $ bfsOn snd [(0,pos)] nexts
     where
      nexts (_, p) | p /= pos && p `elem` crossings = []
      nexts (i, p) = map (i+1,) . filter (`Map.member` input) 
                   $ ordinalNeighbours p

dfs :: ((Int, Point) -> [(Int, Point)]) -> Point -> Point -> [(Int, Point)]
dfs next end start = go Set.empty (0, start)
  where
    go seen x@(i, pos) | pos == end = [x]
    go seen x@(i, pos)
      | Set.member pos seen = []
      | otherwise = do
          n <- next x
          let seen' = Set.insert pos seen
          go seen' n

part2 (mkGraph -> input) = dfs nexts (V2 maxx maxy) (V2 minx miny)
  where
    (minx,miny,maxx,maxy) = findBounds (Map.keys input)
    nexts (i, pos) = map (first (+i)) $ input Map.! pos

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day23.in"
  let (minx,miny,maxx,maxy) = findBounds (Map.keys input)
  print $ last $ filter ((== V2 maxx maxy) . view _2) $ part1 input
  print $ maximumOn fst $ part2 input

parseInput :: String -> Map Point Char
parseInput = parseAsciiMap f
  where f x = guard (x `elem` ".<>v^") *> Just x

-- 2178
-- 6486
