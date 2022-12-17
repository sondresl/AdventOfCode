module Day17 where

import Lib (skipLoop, Point, parseAsciiMap)
import Linear (V2(..), _x, _y)
import Advent.Coord (down, left, right)
import Control.Lens (view)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Wind = (Int, Point -> Point)

fall :: [Set Point] -> [Wind] -> Set Point -> Set Point -> [Set Point]
fall rocks wind current board = if board `Set.disjoint` downOne 
         then blow rocks wind downOne board
         else board' : blow (tail rocks) wind new (trim board')
  where
    downOne = Set.map (+ down) current
    board' = Set.union current board
    mx = (+4) $ highestPoint board'
    new = Set.map (+ V2 0 mx) (head rocks)
    trim ps = foldr Set.delete ps [ V2 x y | V2 x y <- Set.toList ps, y < cutoff ]
      where cutoff = subtract 100 $ highestPoint ps

blow :: [Set Point] -> [Wind] -> Set Point -> Set Point -> [Set Point]
blow rocks ((n, dir):wind) current board
  -- Check whether rock is on the edge, or if shifting it will put it inside existing rocks
  | n `elem` Set.map (view _x) current || not (Set.map dir current `Set.disjoint` board) 
  = fall rocks wind current board
  | otherwise = fall rocks wind (Set.map dir current) board

part2 :: Int -> [Set Point] -> Int
part2 bigNum = highestPoint . skipLoop norm shift bigNum
  where
    shift loopShift looped = Set.map (+ V2 0 y)
      where y = loopShift * looped
    norm ps = (toZeroY, Set.map (+ V2 0 (-toZeroY)) trimmed)
      where
        mx = highestPoint ps
        trimmed = Set.filter ((> mx - 100) . view _y) ps
        toZeroY = minimum . map (view _y) $ Set.toList ps

highestPoint :: Set Point -> Int
highestPoint = maximum . Set.map (view _y)

main :: IO ()
main = do
  input <- map parseWind . init <$> readFile "../data/day17.in"
  let start = Set.fromList $ map (`V2` 0) [1..7]
  let res = start : blow (tail $ cycle rocks) (cycle input) (Set.map (+ V2 0 4) (head rocks)) start
  print $ highestPoint (res !! 2022)
  print $ part2 1000000000000 res

parseWind :: Char -> (Int, Point -> Point)
parseWind '<' = (1, (+ left))
parseWind '>' = (7, (+ right))

rocks :: [Set Point]
rocks = map (Set.map (+ (3 * right)) 
      . Map.keysSet 
      . parseAsciiMap f) [ "####"
                         , ".#.\n###\n.#."
                         , "###\n..#\n..#"
                         , "#\n#\n#\n#"
                         , "##\n##"
                         ]
  where f '#' = Just ()
        f _ = Nothing

-- 3151
-- 1560919540245
