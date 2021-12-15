{-# LANGUAGE TypeApplications #-}
module Day15 where

import Lib (parseAsciiMap, findBounds, neighbours4)
import Linear (V2(..))
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Algorithm.Search (dijkstra)

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

main :: IO ()
main = do
  input <- parseAsciiMap (Just . read @Int . pure) <$> readFile "../data/day15.in"
  let nbs mp v2 = filter (`Map.member` mp) $ neighbours4 v2
      cost mp _ to = mp Map.! to
      run mx my mp = fst . fromJust $ dijkstra (nbs mp) (cost mp) (V2 mx my ==) (V2 0 0)
      (_, _, mx, my) = findBounds $ Map.keys input

  print $ run mx my input

  let input' = bigger input
      (_, _, mx, my) = findBounds $ Map.keys input'
  print $ run mx my input'
    
-- 811
-- 3012
