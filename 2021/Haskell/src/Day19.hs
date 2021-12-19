module Day19 where

import Lib (freqs, combinations, tuple, select)
import Linear (V3(..))
import Control.Monad (guard)
import Data.List.Extra (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec 
    (many1, digit, string, sepEndBy, newline, sepBy1, (<|>), parse, char)

type Beacons = Set (V3 Int)

align :: (Map Int (V3 Int), Beacons) -> [(Int, Beacons)] -> [(Map Int (V3 Int), Beacons)]
align (scanners, acc) new = do
  ((pos, cand), rest) <- select new
  orient <- reorient cand
  (diff, curr) <- shift acc orient
  let res = Set.union curr acc
      scanners' = Map.insert pos diff scanners
  if null rest
     then pure (scanners', res)
     else align (scanners', res) rest

shift :: Beacons -> Beacons -> [(V3 Int, Beacons)]
shift acc new = map (\v3 -> (v3, Set.mapMonotonic (+ v3) new)) . Map.keys $ Map.filter (>= 12) fr
  where
    fr = freqs $ do
      a <- Set.toList acc
      b <- Set.toList new
      pure $ a - b

reorient :: Beacons -> [Beacons]
reorient inp = do
  op <- dir
  dir <- rot
  pure $ Set.map (op . dir) inp
    where 
      dir = [ \(V3 x y z) -> V3 x y z -- face forward
            , \(V3 x y z) -> V3 (-y) x z  -- face left
            , \(V3 x y z) -> V3 (-x) (-y) z  -- face opposite
            , \(V3 x y z) -> V3 y (-x) z  -- face right
            , \(V3 x y z) -> V3 x (-z) y  -- face up
            , \(V3 x y z) -> V3 x z (-y)  -- face down
            ]
      rot = [ \(V3 x y z) -> V3 x y z -- no rot
            , \(V3 x y z) -> V3 z y (-x)  -- rot 90 right
            , \(V3 x y z) -> V3 (-x) y (-z) -- rot 180
            , \(V3 x y z) -> V3 (-z) y x -- rot 90 left
            ]

main :: IO ()
main = do
  (start:rest) <- parseInput <$> readFile "../data/day19.in"
  let (scanners, combined) = head $ align (Map.singleton (fst start) 0, snd start) rest
  print $ Set.size combined
  print . maximum $ do
    (x,y) <- tuple <$> combinations 2 (Map.elems scanners)
    pure $ sum $ abs $ y - x

parseInput :: String -> [(Int, Beacons)]
parseInput = either (error . show) id . traverse (parse p "") . splitOn "\n\n"
  where
    p = (,) <$> (read @Int <$> (string "--- scanner " *> many1 digit))
            <*> (Set.fromList <$> (string " ---" *> newline *> coords))
    coords = sepEndBy ((\[x,y,z] -> V3 x y z) <$> sepBy1 num (char ',')) newline
    neg = negate <$> (char '-' *> num)
    pos = read @Int <$> many1 digit
    num = neg <|> pos

-- 449
-- 13128
