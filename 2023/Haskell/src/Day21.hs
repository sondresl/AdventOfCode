module Day21 where

import Lib
import Advent.Coord
import Advent.Search
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.RawString.QQ
import Text.ParserCombinators.Parsec hiding (count)
import Data.MemoTrie

type Garden = Set Point

part1 :: Garden -> Point -> Int
part1 input start = length . filter (even . fst) . takeWhile ((< 65) . fst) $ walk input [(0, start)]

part2 input start = undefined

walk :: Garden -> [(Int, Point)] -> [(Int, Point)]
walk input = memo $ \starts -> bfsOn snd starts nexts
  where nexts (i, pos) = map (i+1,) . filter (`Set.member` input) $ ordinalNeighbours pos

main :: IO ()
main = do

  let run str input = do
        let ps = Map.keysSet input
            Just (start,_) = find ((== 'S') . snd) $ Map.assocs input
        putStrLn str
        print $ part1 ps start
        -- print $ part2 input
    
  run "\nTest:\n\n" $ parseInput testInput

  input <- parseInput <$> readFile "../data/day21.in"
  run "\nActual:\n\n" input

parseInput = parseAsciiMap f
  where f x = guard (x `elem` ".S") *> Just x

-- 3809

-- parseInput = either (error . show) id . traverse (parse p "") . lines
--   where
--     p = undefined

testInput = [r|...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
|]
