module Day21 where

import Lib (groupBy, iterateN, ordNub, zipWithTail, allNums, select)
import Advent.Coord (Coord)
import Control.Monad (guard, foldM)
import Data.List.Extra (permutations)
import Data.Map (Map)
import qualified Data.Map as Map
import Linear hiding (trace)

numpad :: Map Coord Char
numpad = Map.fromList [
    (V2 0 3, '7') , (V2 1 3, '8') , (V2 2 3, '9')
  , (V2 0 2, '4') , (V2 1 2, '5') , (V2 2 2, '6')
  , (V2 0 1, '1') , (V2 1 1, '2') , (V2 2 1, '3')
                  , (V2 1 0, '0') , (V2 2 0, 'A')
  ]

keypad :: Map Coord Char
keypad = Map.fromList
  [                 (V2 1 1, '^') , (V2 2 1, 'A')
  , (V2 0 0, '<') , (V2 1 0, 'v') , (V2 2 0, '>')
  ]

consec :: Ord a => [a] -> Bool
consec str = let xs = map head $ Lib.groupBy (==) str
              in length xs == length (ordNub xs)

-- new idea
paths :: Map Coord Char -> Map (Char, Char) [String]
paths mp = fmap (take 2 . ordNub) . Map.unionsWith (<>) $ do
  ((start, a), rest) <- select (Map.assocs mp)
  (end,   b)         <- rest
  path <- filter consec $ genPaths start end
  guard $ all (`Map.member` mp) $ foldl (\acc new -> new + head acc : acc) [start] path
  pure $ Map.fromList [ ((a,b), [map toDir path])
                      , ((a,a), [""]) ]

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

typeString :: Map (Char, Char) [String] -> String -> [String]
typeString mp str = let xs = foldM f "" (zipWithTail $ ('A' : str))
                        l = minimum $ map length xs
                     in filter (\x -> length x == l) xs
  where f acc pair = map (\n -> acc <> n <> "A") (mp Map.! pair)

dirMoves, padMoves :: Map (Char, Char) [String]
dirMoves = Map.union hardcoded $ fmap (take 1) (paths keypad)
  where
    hardcoded = Map.fromList [ (('>', '^'), ["<^"])
                             , (('^', '>'), ["v>"])
                             , (('A', 'v'), ["<v"])
                             , (('v', 'A'), ["^>"])]
padMoves = paths numpad

pairs :: String -> Map (Char, Char) Int
pairs str = foldr f Map.empty (zipWithTail ('A' : str))
  where 
    f :: (Char, Char) -> Map (Char, Char) Int -> Map (Char, Char) Int
    f new acc = Map.insertWith (+) new 1 acc

expand :: Map (Char, Char) Int -> [Map (Char, Char) Int]
expand mp = foldM f Map.empty (Map.assocs mp)
  where
    f acc (pair, cnt) = case dirMoves Map.! pair of
             [""] -> [Map.insertWith (+) pair cnt acc]
             xs -> map (\x -> (Map.unionWith (+) acc ((* cnt) <$> pairs (x <> "A")))) xs

part2 :: Int -> String -> Int -- [Map (Char, Char) Int]
part2 n str = num * total
  where 
    start = map pairs $ typeString padMoves str
    num = head $ allNums str
    total = minimum $ map sum $ iterateN n (concatMap expand) start
 
main :: IO ()
main = do
  input <- lines <$> readFile "../data/day21.in"
  print $ sum $ map (part2 2) input
  print $ sum $ map (part2 25) input
    
-- 188398
-- 230049027535970

