module Day20 where

import Lib (Point, count, parseAsciiMap, findBounds, neighbours)
import Linear (V2(..))
import Data.List.Extra (splitOn, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

step :: Int -> Map Int Int -> Map Point Int -> Map (V2 Int) Int
step n algo input = Map.mapWithKey f expanded
  where
    (minx,miny,maxx,maxy) = findBounds $ Map.keys input
    expanded = Map.fromList [ (V2 x y, 0) | x <- [minx - 1..maxx + 1], y <- [miny - 1..maxy + 1]]
    f v2 _ = 
      let ns = map (\v2 -> Map.findWithDefault n v2 input) 
             . sortBy (\(V2 x y) (V2 x' y') -> compare y y' <> compare x x')
             . (v2:) $ neighbours v2
          num = foldl (\acc new -> acc * 2 + new) 0 ns
       in algo Map.! num

main :: IO ()
main = do
  (code, input) <- parseInput <$> readFile "../data/day20.in"
  let twice n m grid = step m code $ step n code grid
      run n = count (== 1) . (!! (n `div` 2)) $ iterate (twice 0 1) input
  print $ run 2
  print $ run 50

parseInput :: String -> (Map Int Int, Map Point Int)
parseInput = f . splitOn "\n\n"
  where
    f [x,y] = (Map.fromList $ zipWith h [0..] x, parseAsciiMap g y)
    g '#' = Just 1
    g '.' = Just 0
    h n '#' = (n, 1)
    h n '.' = (n, 0)

-- 5391
-- 16383
