module Day20 where

import Lib (toMapIndexed, Point, tuple, count, parseAsciiMap, findBounds, neighbours)
import Linear (V2(..))
import Data.Digits (unDigits)
import Data.Tuple.Extra ((***))
import Data.List.Extra (splitOn, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

step :: Int -> Map Int Int -> Map Point Int -> Map (V2 Int) Int
step n algo input = foldMap f expanded
  where
    (minx,miny,maxx,maxy) = findBounds $ Map.keys input
    expanded = [ V2 x y | x <- [minx - 1..maxx + 1], y <- [miny - 1..maxy + 1]]
    f v2 = Map.singleton v2 $ algo Map.! num
      where num = unDigits 2
                . map (\v2 -> Map.findWithDefault n v2 input)
                . sortBy (\(V2 x y) (V2 x' y') -> compare y y' <> compare x x')
                . (v2:)
                $ neighbours v2

main :: IO ()
main = do
  (code, input) <- parseInput <$> readFile "../data/day20.in"
  let run n = count (== 1) . (!! (n `div` 2))
            $ iterate (step 1 code . step 0 code) input
  print $ run 2
  print $ run 50

parseInput :: String -> (Map Int Int, Map Point Int)
parseInput = f . splitOn "\n\n"
  where
    f = (toMapIndexed . map g *** parseAsciiMap (Just . g)) . tuple
    g '#' = 1
    g '.' = 0

-- 5391
-- 16383
