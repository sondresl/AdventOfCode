module Day14 where

import Lib
import Advent.Coord
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

part1 input = product . freqs $ filter (/= 0) bots
  where
    bots = map (score bounds . iterateN 100 (move bounds)) input
    bounds = V2 101 103

part2 :: [Robot] -> Maybe (Int, [Robot])
part2 input = find tree . zip [0..] $ iterate (map (move bounds)) input
  where 
    bounds = V2 101 103
    tree (ix, bots) = all hasNeighbour botSet
      where 
        botSet = Set.fromList (map fst bots)
        hasNeighbour b = any (`Set.member` botSet) $ neighbours b

part3 :: [Robot] -> [[Point]]
part3 input = map (map fst) $ iterate (map (move bounds)) input
  where 
    bounds = V2 101 103
    tree (ix, bots) = all hasNeighbour botSet
      where 
        botSet = Set.fromList (map fst bots)
        hasNeighbour b = any (`Set.member` botSet) $ neighbours b

move :: Point -> Robot -> Robot
move bounds (loc, dir) = (loc', dir)
  where loc' = liftA2 mod (loc + dir) bounds

score :: Point -> Robot -> Int
score bounds@(V2 bx by) (V2 px py, _)
  | px == bx `div` 2 || py == by `div` 2 = 0
  | px < bx `div` 2 && py < by `div` 2   = 1
  | px > bx `div` 2 && py < by `div` 2   = 2
  | px > bx `div` 2 && py > by `div` 2   = 3
  | px < bx `div` 2 && py > by `div` 2   = 4

main :: IO ()
main = do

  let run str input = do
        putStrLn str
        let r = (V2 2 4, V2 2 (-3))
            -- bounds = V2 101 103
            bounds = V2 11 7
        print $ part1 input
        -- print $ fst <$> part2 input
        -- mapM_ (putStrLn . display) $ part3 input
        let result = drop 939 $ part3 input
        let button n xs = do
             _ <- getLine
             putStrLn . display $ head xs
             print n
             button (n + 103) (drop 103 xs)
        button 939 result

    

  input <- parseInput <$> readFile "../data/day14.in"
  run "\nActual:\n\n" input

parseInput :: String -> [(Point, Point)]
parseInput = map (f . allNums) . lines
  where f [px, py, vx, vy] = (V2 px py, V2 vx vy)

-- 219512160
-- not 45

testInput = [r|p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
|]


