module Day19 where

import Lib (combinations, tuple, select)
import Linear (V3(..))
import Control.Monad (guard)
import Data.List.Extra (nub, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec hiding (count)

type Beacons = Set (V3 Int)

-- Return distances > 12 frequent or return combined beacons
align :: (Map Int (V3 Int), Beacons) -> [(Int, Beacons)] -> [(Map Int (V3 Int), Beacons)]
align (scanners, acc) new = do
  ((pos, cand),rest) <- select new
  orient <- reorient cand
  anchor <- Set.toList acc
  (diff, curr) <- move orient anchor
  let overlap = Set.size $ Set.intersection curr acc
  guard $ overlap >= 12
  let res = Set.union curr acc
      scanners' = Map.insert pos diff scanners
  if null rest
     then pure (scanners', res)
     else align (scanners', res) rest

move :: Beacons -> V3 Int -> [(V3 Int, Beacons)]
move bs v3 = do
  b <- Set.toList bs
  let diff = v3 - b
  pure $ (diff, Set.map (+ diff) bs)

reorient :: Beacons -> [Beacons]
reorient inp = nub $ do
  op <- dir
  dir <- rot
  pure $ Set.map (op . dir) inp
    where 
      dir =
        [ \(V3 x y z) -> V3 x y z -- face forward
        , \(V3 x y z) -> V3 (negate y) x z  -- face left
        , \(V3 x y z) -> V3 (negate x) (negate y) z  -- face opposite
        , \(V3 x y z) -> V3 y (negate x) z  -- face right
        , \(V3 x y z) -> V3 x (negate z) y  -- face up
        , \(V3 x y z) -> V3 x z (negate y)  -- face down
        ]
      rot =
        [ \(V3 x y z) -> V3 x y z -- no rot
        , \(V3 x y z) -> V3 z y (negate x)  -- rot 90 right
        , \(V3 x y z) -> V3 (negate x) y (negate z) -- rot 180
        , \(V3 x y z) -> V3 (negate z) y x -- rot 90 left
        ]

main :: IO ()
main = do

  let run str file = do
        (start:rest) <- parseInput <$> readFile file
        putStrLn str

        let (scanners, combined) = head $ align (Map.singleton (fst start) 0, snd start) rest
        print $ Set.size combined
        print . maximum $ do
          (x,y) <- tuple <$> combinations 2 (Map.elems scanners)
          pure $ sum $ abs $ y - x

  run "\nTest:\n\n" "../data/test.in"
  -- run "\nActual:\n\n" "../data/day19.in"

parseInput :: String -> [(Int, Beacons)]
parseInput = either (error . show) id . traverse (parse p "") . splitOn "\n\n"
  where
    p = do
      string "--- scanner "
      scannerId <- read @Int <$> many1 digit
      string " ---" *> newline
      cs <- coords
      pure (scannerId, Set.fromList cs)
    coords = sepEndBy ((\[x,y,z] -> V3 x y z) <$> sepBy1 num (char ',')) newline
    neg = negate <$> (char '-' *> num)
    pos = read @Int <$> many1 digit
    num = neg <|> pos

-- 449
-- 13128
