{-# LANGUAGE MultiWayIf #-}
module Day15 where

import Lib (parseAsciiMap, findBounds, neighbours4)
import Linear (V2(..))
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.PQueue.Prio.Min as PQ

bigger :: Map (V2 Int) Int -> Map (V2 Int) Int
bigger mp = foldMap f (Map.toList mp)
  where
    times = (+1) . maximum . map ((\(V2 x y) -> x) . fst) $ Map.toList mp
    f (V2 x y, v) = Map.fromList $ do 
      plusx <- [0.. 4]
      plusy <- [0.. 4]
      pure (V2 (x + times * plusx) (y + times * plusy), add v (plusx + plusy))
    add x v 
      | x + v > 9 = 1 + ((x + v) `mod` 10)
      | otherwise = x + v

shortestPath :: Map (V2 Int) Int -> Int
shortestPath mp = go Set.empty (PQ.singleton 0 (V2 0 0))
  where
    (_, _, mx, my) = findBounds $ Map.keys mp
    go seen q = 
      let ((cost, here), q') = PQ.deleteFindMin q
          new = mapMaybe (adj cost) $ neighbours4 here
          adj x v2 = (,v2) . (+x) <$> Map.lookup v2 mp
       in if | here == V2 mx my -> cost
             | here `Set.member` seen -> go seen q'
             | otherwise -> go (Set.insert here seen) $ foldr (uncurry PQ.insert) q' new

main :: IO ()
main = do
  input <- parseAsciiMap (Just . read @Int . pure) <$> readFile "../data/day15.in"
  print $ shortestPath input
  print $ (shortestPath . bigger) input
    
-- 811
-- 3012
