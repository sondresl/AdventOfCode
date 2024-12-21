module Day21 where

import Lib
import Advent.Coord
import Advent.Search
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Text.RawString.QQ
import Text.ParserCombinators.Parsec hiding (count)
import Linear hiding (trace)

import Debug.Trace

flipMap :: (Ord a, Ord b) => Map a b -> Map b a
flipMap = Map.fromList . map (\(a,b) -> (b,a)) . Map.assocs

-- new idea
dumbPaths :: Map Coord Char -> Map (Char, Char) [String]
dumbPaths mp = Map.unionsWith (<>) $ do
  (start, a) : rest <- tails (Map.assocs mp)
  (end,   b)        <- rest
  path <- genPaths start end
  guard $ all (`Map.member` mp) $ foldr (\new acc -> new + head acc : acc) [start] path
  let path' = trace ("path: " <> show path <> " | " <> show (a, b)) map toDir path
  pure $ Map.fromList [ ((a,b), [map toDir path])
                      , ((b,a), [map (toDir . turnAround) path]) ]

toDir = \case
  V2 1 0    -> '>'
  V2 0 1    -> '^'
  V2 (-1) 0 -> '<'
  V2 0 (-1) -> 'v'

dir x
  | x > 0 = 1
  | x < 0 = (-1)
  | otherwise = 0

genPaths from to = ordNub $ permutations (xs <> ys)
  where
    V2 x y = to - from
    xs = replicate (abs x) (V2 (dir x) 0)
    ys = replicate (abs y) (V2 0 (dir y))


typeString mp str = ordNub $ foldM f "" (zipWithTail $ ('A' : str))
  -- where f acc pair = map (\n -> acc <> n <> "A") (mp Map.! pair)
  where f acc pair = map (\n -> acc <> n <> "A") (Map.findWithDefault [""] pair mp)

dirMoves, padMoves :: Map (Char, Char) [String]
dirMoves = dumbPaths (flipMap directional)
padMoves = dumbPaths (flipMap keypad)

-- part1 s = length res * num s
part1 s = (res, num s, length res, length res * num s)
  where
    res = go s
    num = head . allNums
    go = minimumOn length 
       . concatMap (typeString dirMoves) 
       . concatMap (typeString dirMoves) 
       . typeString padMoves

main :: IO ()
main = do
-- 935A 319A 480A 789A 176A

  let run str input = do
        putStrLn str
        print $ part1 "319A"
        -- print $ part1 input
    
  run "\nTest:\n\n" $ parseInput testInput

  -- input <- parseInput <$> readFile "../data/day21.in"
  -- run "\nActual:\n\n" input

keypad = Map.fromList
  [ ('0', V2 1 0) , ('A', V2 2 0) , ('1', V2 0 1)
  , ('2', V2 1 1) , ('3', V2 2 1) , ('4', V2 0 2)
  , ('5', V2 1 2) , ('6', V2 2 2) , ('7', V2 0 3)
  , ('8', V2 1 3) , ('9', V2 2 3)
  ]

directional = Map.fromList
  [                 ('^', V2 1 1) , ('A', V2 2 1)
  , ('<', V2 0 0) , ('v', V2 1 0) , ('>', V2 2 0)
  ]

parseInput = lines

-- parseInput = either (error . show) id . traverse (parse p "") . lines
--   where
--     p = undefined

testInput = [r|029A
980A
179A
456A
379A
|]
