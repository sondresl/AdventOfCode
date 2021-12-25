module Day17 where

import Lib (takeUntil, allNums)
import Data.Maybe (fromJust, mapMaybe)
import Control.Lens (maximumOf, _1, _2, each)
import Text.ParserCombinators.Parsec (many1, digit, char, parse, string, (<|>))

type Point = (Int, Int)
type Bound = (Point, Point)

shoot :: Bound -> Point -> Maybe [Bound]
shoot ((tx,tx'), (ty, ty')) (dx,dy) = path 
  where
    step ((x, y), (dx, dy)) = ((x + dx, y + dy), (max 0 (dx - 1), dy - 1))
    between x tx ty = x >= min tx ty && x <= max tx ty
    path = (\x -> if length x == bound then Nothing else Just x)
         . take bound
         . takeUntil (\((x,y),(dx,dy)) -> between x tx tx' && between y ty ty')
         $ iterate step ((0,0), (dx, dy))
           where bound = 400

main :: IO ()
main = do
  input@((_, mx), (miy, mxy)) <- parseInput <$> readFile "../data/day17.in"

  let paths = mapMaybe (fmap . (,) <*> shoot input) $ do
        x <- [0..mx + 1]
        y <- [min miy mxy..(abs (min miy mxy) - 1)]
        pure (x,y)

  print . maximumOf (each . _2 . each . _1 . _2) $ paths
  print $ length paths

parseInput :: String -> ((Int, Int), (Int, Int))
parseInput str = let [x1,x2,y1,y2] = allNums str in ((x1,x2), (y1, y2))

-- Just 3160
-- 1928
