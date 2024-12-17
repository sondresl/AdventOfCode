module Day22 where

import Lib (parseAsciiMap, Point)
import Linear (V2(..), _y)
import Advent.Coord (right, left, turnRight, turnLeft)
import Control.Lens (view)
import Data.List.Extra (splitOn, minimumOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec (digit, many1, parse, oneOf, (<|>))

data Tile = Open | Wall deriving (Show, Eq, Ord)
data Rot = R | L deriving (Show, Eq, Ord, Read)
type Area = Map Point Tile

data Log = Log
  { area :: Area
  , position :: Point
  , dir :: Point
  } deriving (Show, Eq, Ord)


moveForward :: Log -> Log
moveForward (Log mp pos dir)
  | Map.lookup (pos + dir) mp == Just Wall = Log mp pos dir
  | (pos + dir) `Map.notMember` mp = 
    let otherSide = last $ takeWhile (`Map.member` mp) $ iterate (subtract dir) pos 
     in if Map.lookup otherSide mp == Just Wall then Log mp pos dir else Log mp otherSide dir
  | otherwise = Log mp (pos + dir) dir

moveCube :: Log -> Log
moveCube (Log mp pos dir)
  | Map.lookup (pos + dir) mp == Just Wall = Log mp pos dir
  | (pos + dir) `Map.notMember` mp, Just (new, dir') <- findEdge pos dir = 
    if mp Map.! new == Wall 
       then Log mp pos dir
       else Log mp new dir'
  | otherwise = Log mp (pos + dir) dir

down = V2 0 1
up   = V2 0 (-1)

-- Return new point and new direction
findEdge :: Point -> Point -> Maybe (Point, Point)
findEdge  pos@(V2 x y) old
  -- Front Right
  | old == right, pos `elem` mkEdge [99]       [50..99]   = Just (V2 (mapPos [50..99] [100..149] y) 49, up)
  | old == down,  pos `elem` mkEdge [100..149] [49]       = Just (V2 99 (mapPos [100..149] [50..99] x), left)
  -- Front Left
  | old == left,  pos `elem` mkEdge [50]       [50..99]   = Just (V2 (mapPos [50..99] [0..49] y) 100, down) 
  | old == up,    pos `elem` mkEdge [0..49]    [100]      = Just (V2 50 (mapPos [0..49] [50..99] x), right)
  -- Back Under
  | old == right, pos `elem` mkEdge [49]       [150..199] = Just (V2 (mapPos [150..199] [50..99] y) 149, up)
  | old == down,  pos `elem` mkEdge [50..99]   [149]      = Just (V2 49 (mapPos [50..99] [150..199] x), left)
  -- Right Under
  | old == right, pos `elem` mkEdge [149]      [0..49]    = Just (V2 99 (mapPos [49,48..0] [100..149] y), left)
  | old == right, pos `elem` mkEdge [99]       [100..149] = Just (V2 149 (mapPos [100..149] [49,48..0] y), left)
  -- Right Back
  | old == up,    pos `elem` mkEdge [100..149] [0]        = Just (V2 (mapPos [100..149] [0..49] x) 199, up)
  | old == down,  pos `elem` mkEdge [0..49]    [199]      = Just (V2 (mapPos [0..49] [100..149] x) 0, down)
  -- Top Back
  | old == up,    pos `elem` mkEdge [50..99]   [0]        = Just (V2 0 (mapPos [50..99] [150..199] x), right)
  | old == left,  pos `elem` mkEdge [0]        [150..199] = Just (V2 (mapPos [150..199] [50..99] y) 0, down)
  -- Top Left
  | old == left,  pos `elem` mkEdge [50]       [0..49]    = Just (V2 0 (mapPos [49,48..0] [100..149] y), right)
  | old == left,  pos `elem` mkEdge [0]        [100..149] = Just (V2 50 (mapPos [100..149] [49,48..0] y), right)
  | otherwise = Nothing

mkEdge :: [Int] -> [Int] -> [Point]
mkEdge xs ys = [ V2 x y | x <- xs, y <- ys ]

mapPos :: [Int] -> [Int] -> Int -> Int
mapPos xs ys x = let Just v = lookup x (zip xs ys) in v

score :: Log -> Int
score (Log _ (V2 x y) dir) = 1000 * (y + 1) + 4 * (x + 1) + v
  where v = case dir of
               V2 1 0    -> 0
               V2 (-1) 0 -> 2
               V2 0 1    -> 1
               V2 0 (-1) -> 3
               _ -> error "score"

part1 :: Log -> Either Int Rot -> Log
part1 log@(Log mp pos dir) = \case
  (Left i)  -> iterate moveForward log !! i
  (Right R) -> Log mp pos (turnLeft dir)
  (Right L) -> Log mp pos (turnRight dir)

part2 :: Log -> Either Int Rot -> Log
part2 log@(Log mp pos dir) = \case
  (Left i)  -> iterate moveCube log !! i
  (Right R) -> Log mp pos (turnLeft dir)
  (Right L) -> Log mp pos (turnRight dir)

main :: IO ()
main = do
  [mp', path'] <- splitOn "\n\n" <$> readFile "../data/day22.in"
  let mp = parseInput mp'
      path = parsePath path'
      start mp = minimumOn (view _y) . map fst . filter ((==Open) . snd) $ Map.toList mp 
  print . score $ foldl part1 (Log mp (start mp) right) path
  print . score $ foldl part2 (Log mp (start mp) right) path

parsePath :: String -> [Either Int Rot]
parsePath = concat . either (error . show) id . traverse (parse p "") . lines
  where
    p = many1 ((Right . read . pure <$> oneOf "RL") <|> (Left . read <$> many1 digit))

parseInput :: String -> Area
parseInput = parseAsciiMap f
  where 
    f '.' = Just Open
    f '#' = Just Wall
    f _ = Nothing

-- 55244
-- 123149
