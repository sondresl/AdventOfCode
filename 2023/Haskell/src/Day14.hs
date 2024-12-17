module Day14 where

import Lib
import Advent.Coord
import Data.Maybe
import Control.Lens
import Control.Monad.State
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Text.RawString.QQ
import Text.ParserCombinators.Parsec hiding (count)
import Linear

data Stone = Cube | Ball
  deriving (Show, Eq, Ord)

stepNorth input = Map.mapKeys f input
  where
    f pos@(V2 x 0) = pos
    f pos@(V2 x y) = case Map.lookup pos input of
                       Just Ball | Map.member (V2 x (pred y)) input -> pos
                       Just Ball -> V2 x (pred y)
                       Just Cube -> pos

stepSouth input = Map.mapKeys f input
  where
    maxy = maximum . map (view _y) $ Map.keys input
    f pos@(V2 x y) | y == maxy = pos
    f pos@(V2 x y) = case Map.lookup pos input of
                       Just Ball | Map.member (V2 x (succ y)) input -> pos
                       Just Ball -> V2 x (succ y)
                       Just Cube -> pos

stepWest input = Map.mapKeys f input
  where
    f pos@(V2 0 y) = pos
    f pos@(V2 x y) = case Map.lookup pos input of
                       Just Ball | Map.member (V2 (pred x) y) input -> pos
                       Just Ball -> V2 (pred x) y
                       Just Cube -> pos

stepEast input = Map.mapKeys f input
  where
    maxx = maximum . map (view _x) $ Map.keys input
    f pos@(V2 x y) | x == maxx = pos
    f pos@(V2 x y) = case Map.lookup pos input of
                       Just Ball | Map.member (V2 (succ x) y) input -> pos
                       Just Ball -> V2 (succ x) y
                       Just Cube -> pos

part1 :: Map Point Stone -> Int
part1 input = score res
  where
    Just res = firstRepeat $ iterate stepNorth input

part2 :: Map Point Stone -> Int
part2 input = score $ skipLoop (0,) (\_ _ a -> a) 1000000000 $ forever input
  where
    forever input = scanl (foldl f) input cy
     where
       cy = repeat [stepNorth, stepWest, stepSouth, stepEast]
       f acc fun = let Just res = firstRepeat (iterate fun acc) in res

score input = sum . map ((maxY -) . view _y) $ Map.keys $ Map.filter (== Ball) input
  where maxY = succ . maximum . map (view _y) $ Map.keys input

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day14.in"
  print $ part1 input
  print $ part2 input

parseInput :: String -> Map Point Stone
parseInput = parseAsciiMap $ \case
               'O' -> Just Ball
               '#' -> Just Cube
               _ -> Nothing
-- 112773
-- 98894
