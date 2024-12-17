module Day24 where

import Lib (parseAsciiMap, Point, ordNub, findBounds, ordinalNeighbours)
import Advent.Coord (down, up, right, left)
import Linear (V2(..))
import Data.List.Extra (find)
import Data.Map (Map)
import qualified Data.Map as Map

type Grove = Map Point Blizzard

data Blizzard = Wall | Blizzard [Point]
  deriving (Show, Eq, Ord)

instance Semigroup Blizzard where
  Wall <> _ = Wall
  _ <> Wall = Wall
  Blizzard xs <> Blizzard ys = Blizzard (xs <> ys)

type State = (Int, [Point], Grove)

moveBlizzard :: Grove -> Grove
moveBlizzard mp = Map.unionsWith (<>) $ map f (Map.toList mp)
  where 
    f (pos, Wall) = Map.singleton pos Wall
    f (pos, Blizzard xs) = Map.fromList $ map (\d -> (around pos d, Blizzard [d])) xs
    around pos dir
      | Map.lookup (pos + dir) mp == Just Wall = last 
                                               . takeWhile (\x -> Map.lookup x mp /= Just Wall) 
                                               $ iterate (subtract dir) pos
      | Map.lookup (pos + dir) mp /= Just Wall = pos + dir

move :: [Point] -> State -> State
move outside (steps, pos, mp) = 
    let mp' = moveBlizzard mp
        pos' = ordNub $ concatMap (\x -> x : ordinalNeighbours x) pos
        pos'' = filter (`notElem` outside) $ filter (`Map.notMember` mp') pos'
    in (steps + 1, pos'', mp')

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day24.in"
  let (minx, miny, maxx, maxy) = findBounds $ Map.keys input
      openingTop = V2 (minx + 1) miny
      openingBot = V2 (maxx - 1) maxy
      finished end (_, ps, _) = end `elem` ps
      compute end start mp = find (finished end) 
                           $ iterate (move [openingTop + down, openingBot + up]) (0, [start], mp)
      Just (s  ,_,mp' ) = compute openingBot openingTop input
      Just (s' ,_,mp'') = compute openingTop openingBot mp'
      Just (s'',_,_   ) = compute openingBot openingTop mp''
  print s
  print (s + s' + s'')

parseInput :: String -> Grove
parseInput = parseAsciiMap f
  where
    f '#' = Just Wall
    f '>' = Just (Blizzard [right])
    f '<' = Just (Blizzard [left])
    f 'v' = Just (Blizzard [up])
    f '^' = Just (Blizzard [down])
    f _ = Nothing

-- 255
-- 809
