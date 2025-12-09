module Day09 where

import Lib (allNums, combinations, tuple, zipWithTail')
import Advent.Coord (Coord)
import qualified Data.Map as Map
import Linear (V2(..))

area :: Coord -> Coord -> Int
area (V2 x y) (V2 a b) = (abs (a - x) + 1) * (abs (b - y) + 1)

part2 :: [Coord] -> Int
part2 corners = maximum . map (uncurry area) $ filter (uncurry isValid) combos
  where
    lines = zipWithTail' corners
    vertical = filter (\(V2 x _, V2 x' _) -> x == x') lines
    horizontal = filter (\(V2 _ y, V2 _ y') -> y == y') lines
    combos = map tuple $ combinations 2 corners

    okHori top@(V2 x y, V2 x' y') bot@(V2 f g, V2 f' g') (V2 a b, V2 a' b') =
       (min b b' >= y || max b b' <= g) || (a <= x || a >= x')

    okVert left@(V2 x y, V2 x' y') right@(V2 f g, V2 f' g') (V2 a b, V2 a' b') =
      max a a' <= x || min a a' >= f || b <= y || b >= y'

    isValid (V2 x y) (V2 x' y') | x == x' || y == y' = False
    isValid (V2 x y) (V2 x' y') =
      let top = (V2 (min x x') (max y y'), V2 (max x x') (max y y'))
          bot = (V2 (min x x') (min y y'), V2 (max x x') (min y y'))
          left = (V2 (min x x') (min y y'), V2 (min x x') (max y y'))
          right = (V2 (max x x') (min y y'), V2 (max x x') (max y y'))
       in    all (okHori top bot)  vertical
          && all (okVert left right) horizontal

main :: IO ()
main = do
  input <- map (uncurry V2 . tuple . allNums) . lines <$> readFile "../data/day09.in"
  print . maximum . map (uncurry area . tuple) $ combinations 2 input
  print $ part2 input

-- 4739623064
-- 1654141440
