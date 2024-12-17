module Day15 where

import Lib
import Advent.Coord (Coord, north, south, east, west)
import Data.List.Extra (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Linear (V2(V2))

data Tile = Wall | Robot | Food | Space | FoodLeft | FoodRight
  deriving (Show, Eq, Ord)

data Warehouse = WH
  { robot :: Coord
  , house :: Map Coord Tile
  }
  deriving (Show, Eq)

findFood :: Warehouse -> Coord -> Maybe (Map Coord Tile)
findFood (WH robot house) dir = go (robot + dir)
  where
    go pos = case Map.lookup pos house of
        Just Wall -> Nothing
        Nothing -> Just Map.empty
        Just Food -> (<> (Map.singleton pos Food)) <$> go (pos + dir)
        Just FoodLeft  -> do
          l <- if (dir `elem` [north, south]) then go (pos + dir) else Just Map.empty 
          r <- go (pos + east + dir)
          pure $ l <> r <> (Map.fromList [(pos, FoodLeft), (pos + east, FoodRight)])
        Just FoodRight -> do
          l <- if (dir `elem` [north, south]) then go (pos + dir) else Just Map.empty 
          r <- go (pos + west + dir)
          pure $ l <> r <> (Map.fromList [(pos, FoodRight), (pos + west, FoodLeft)])

move :: Warehouse -> Coord -> Warehouse
move wh@(WH robot house) dir = case findFood wh dir of
  Nothing -> wh
  Just ps -> WH (robot + dir)
                (Map.union (Map.mapKeys (+ dir) ps) 
                           (Map.withoutKeys house (Map.keysSet ps)))

score :: Warehouse -> Int
score (WH _ house) = sum [ y * 100 + x | (V2 x y, food) <- Map.assocs house
                         , food == Food || food == FoodLeft ]

main :: IO ()
main = do
  (wh, dirs) <- parseInput <$> readFile "../data/day15.in"
  print . score $ foldl move wh dirs
  print . score $ foldl move (expand wh) dirs

expand :: Warehouse -> Warehouse
expand (WH robot house) = WH (robot * V2 2 1) (Map.foldMapWithKey f house)
  where
    f k v = let (l, r) = case v of
                   Food -> (FoodLeft, FoodRight)
                   Wall -> (Wall, Wall)
             in Map.fromList [(k * V2 2 1, l), (k * V2 2 1 + V2 1 0, r)]

parseInput :: String -> (Warehouse, [Coord])
parseInput input = (WH robot (Map.delete robot mp), dirs)
  where
    [top, bot] = splitOn "\n\n" input
    robot = head [ k | (k, Robot) <- Map.assocs mp ]
    mp = flip parseAsciiMap top $ \case
      '#' -> Just Wall
      '@' -> Just Robot
      'O' -> Just Food
      _   -> Nothing
    dirs = flip map (filter (/= '\n') bot) $ \case
      '^' -> north
      '<' -> west
      '>' -> east
      'v' -> south

-- 1563092
-- 1582688
