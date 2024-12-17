module Day14 where

import Lib
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
import Linear hiding (trace)

type Robot = (Point, Point)

bounds :: Point
bounds = V2 101 103

part1 :: [Robot] -> Int
part1 = product . freqs . mapMaybe (score . fst . iterateN 100 move)
  where
    V2 bx by = bounds
    score (V2 px py)
      | px < bx `div` 2 && py < by `div` 2 = Just 1
      | px > bx `div` 2 && py < by `div` 2 = Just 2
      | px > bx `div` 2 && py > by `div` 2 = Just 3
      | px < bx `div` 2 && py > by `div` 2 = Just 4
      | otherwise = Nothing

part2 :: [Robot] -> Maybe Int
part2 input = fst <$> (find tree . zip [0..] $ iterate (map move) input)
  where 
    tree (ix, bots) = (> 50) . maximum $ map Set.size $ regions botSet
      where 
        botSet = Set.fromList (map fst bots)
        regions = floodFill (filter (`Set.member` botSet) . ordinalNeighbours)
        hasNeighbour b = any (`Set.member` botSet) $ neighbours b

move :: Robot -> Robot
move (loc, dir) = (loc', dir)
  where loc' = liftA2 mod (loc + dir) bounds

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day14.in"
  print $ part1 input
  print $ part2 input

  -- Show the tree:
  -- putStrLn . display . map (over _y (`subtract` 103) . fst) $ iterateN 6398 (map (move bounds)) input

parseInput :: String -> [Robot]
parseInput = map (f . allNums) . lines
  where f [px, py, vx, vy] = (V2 px py, V2 vx vy)

-- 219512160
-- 6398
-- xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
-- x                             x
-- x                             x
-- x                             x
-- x                             x
-- x              x              x
-- x             xxx             x
-- x            xxxxx            x
-- x           xxxxxxx           x
-- x          xxxxxxxxx          x
-- x            xxxxx            x
-- x           xxxxxxx           x
-- x          xxxxxxxxx          x
-- x         xxxxxxxxxxx         x
-- x        xxxxxxxxxxxxx        x
-- x          xxxxxxxxx          x
-- x         xxxxxxxxxxx         x
-- x        xxxxxxxxxxxxx        x
-- x       xxxxxxxxxxxxxxx       x
-- x      xxxxxxxxxxxxxxxxx      x
-- x        xxxxxxxxxxxxx        x
-- x       xxxxxxxxxxxxxxx       x
-- x      xxxxxxxxxxxxxxxxx      x
-- x     xxxxxxxxxxxxxxxxxxx     x
-- x    xxxxxxxxxxxxxxxxxxxxx    x
-- x             xxx             x
-- x             xxx             x
-- x             xxx             x
-- x                             x
-- x                             x
-- x                             x
-- x                             x
-- xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

