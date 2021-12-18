{-# LANGUAGE MultiWayIf #-}
module Day15 where

import Lib (parseAsciiMap, findBounds, neighbours4)
import Linear (V2(..))
import Data.Maybe (fromJust)
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

shortestPath :: V2 Int -> V2 Int -> Map (V2 Int) Int -> Int
shortestPath from to mp = go Set.empty (PQ.singleton 0 from)
  where
    (_, _, mx, my) = findBounds $ Map.keys mp
    go seen q = 
      let ((cost, here), q') = PQ.deleteFindMin q
          new :: [(Int, V2 Int)]
          new = map (adj cost) $ filter (`Map.member` mp) $ neighbours4 here
          adj x v2 = (x + mp Map.! v2, v2)
          seen' = Set.insert here seen
       in if | here == to -> cost
             | here `Set.member` seen -> go seen q'
             | otherwise -> go seen' $ foldr (uncurry PQ.insert) q' new

main :: IO ()
main = do
  input <- parseAsciiMap (Just . read @Int . pure) <$> readFile "../data/day15.in"
  let (_, _, mx, my) = findBounds $ Map.keys input
  print $ shortestPath (V2 0 0) (V2 mx my) input

  let big = bigger input
      (_, _, mx, my) = findBounds $ Map.keys big
  print $ shortestPath (V2 0 0) (V2 mx my) big
    
-- 811
-- 3012
