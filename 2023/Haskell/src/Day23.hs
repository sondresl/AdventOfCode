module Day23 where

import Lib (Point, ordinalNeighbours, findBounds, parseAsciiMap)
import Advent.Search (bfsOn)
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as Map
import Linear (V2(V2))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Tuple.Extra (first)

type Graph = Map Point [(Int, Point)]

canWalk :: Map Point Char -> Point -> Point -> Bool
canWalk input from to = (to - from, input Map.! to) `notElem`
  [(V2 0 1, '^'), (V2 1 0, '<'), (V2 0 (-1),'v'), (V2 (-1) 0, '>')]

mkGraph :: Map Point Char -> (Point -> Point -> Bool) -> Graph
mkGraph input f = foldMap steps crossings
  where
    (minx,miny,maxx,maxy) = findBounds (Map.keys input)
    crossings = ([V2 1 0, V2 maxx maxy] <>) 
              . filter ((>2) . length . filter (`Map.member` input) . ordinalNeighbours) 
              $ Map.keys input
    steps pos = Map.singleton pos . filter ((`elem` crossings) . snd) . tail $ bfsOn snd [(0,pos)] nexts
     where
      nexts (_, p) | p /= pos && p `elem` crossings = []
      nexts (i, p) = map (i+1,) 
                   . filter (f pos)
                   . filter (`Map.member` input) 
                   $ ordinalNeighbours p

solve :: Graph -> Int
solve input = maximum . map fst $ longestPath nexts end (V2 1 0)
  where
    end = maximum $ Map.keys input
    nexts (i, pos) = map (first (+i)) $ input Map.! pos
    longestPath next end start = go Set.empty (0, start)
      where
        go seen x@(i, pos) | pos == end = [x]
        go seen x@(i, pos)
          | Set.member pos seen = []
          | otherwise = do
              n <- next x
              let seen' = Set.insert pos seen
              go seen' n

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day23.in"
  print $ solve (mkGraph input (canWalk input))
  print $ solve (mkGraph input (const $ const True))

parseInput :: String -> Map Point Char
parseInput = parseAsciiMap f
  where f x = guard (x `elem` ".<>v^") *> Just x

-- 2178
-- 6486
