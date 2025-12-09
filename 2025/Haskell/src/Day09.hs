module Day09 where

import Lib (allNums, combinations, tuple, zipWithTail', (.:))
import Advent.Coord (Coord)
import qualified Data.Map as Map
import Linear (V2(..))

area :: Coord -> Coord -> Int
area = product .: ((+1) .: liftA2 (abs .: (-)))

part2 :: [Coord] -> [(Coord, Coord)]
part2 corners = filter (uncurry isValid) . map tuple $ combinations 2 corners
  where
    lines = map (\(from, to) -> (liftA2 min from to, liftA2 max from to)) $ zipWithTail' corners

    isValid :: Coord -> Coord -> Bool
    isValid from to = all (valid mi ma) lines
      where
        mi = liftA2 min from to
        ma = liftA2 max from to 

    valid :: Coord -> Coord -> (Coord, Coord) -> Bool
    valid from to (mi, ma) = or (liftA2 (<=) to mi) || or (liftA2 (>=) from ma)

main :: IO ()
main = do
  input <- map (uncurry V2 . tuple . allNums) . lines <$> readFile "../data/day09.in"
  let solve = maximum . map (uncurry area)
  print . solve . map tuple . combinations 2 $ input
  print . solve . part2 $ input

-- 4739623064
-- 1654141440
