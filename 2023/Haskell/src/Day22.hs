module Day22 where

import Lib (allNums, lineSegment, invertMap)
import Advent.Coord (below)
import Data.Maybe (listToMaybe, mapMaybe)
import Control.Lens (view, Bifunctor(bimap), Field1(_1))
import Control.Monad (guard)
import Data.List.Extra (nub, minimumOn, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V3(..))

type Brick = (V3 Int, V3 Int)

fallFast :: Set Brick -> Map Brick (Set (V3 Int))
fallFast bs = go bs Set.empty Map.empty
  where
    go old segs supports | Set.null old = supports
    go old segs supports = case down lowest of
                   Nothing -> go (Set.delete lowest old) 
                                 (segs <> Set.fromList (uncurry lineSegment lowest))
                                 (Map.insert lowest (Set.intersection (Set.fromList $ map (+ below) $ uncurry lineSegment lowest) segs) supports)
                   Just (_,p,ls,supportedBy) -> do
                     go (Set.delete lowest old) (segs <> ls) (Map.insert p supportedBy supports)
      where
        lowest = minimumOn (\(V3 _ _ z, V3 _ _ c) -> min z c) $ Set.toList old
        down d = listToMaybe
               . reverse
               . takeWhile (view _1) 
               . map f
               . takeWhile (\(V3 _ _ z, V3 _ _ c) -> z > 0 && c > 0) 
               $ iterate (bimap (below +) (below +)) d
        f cand = let ls = Set.fromList $ uncurry lineSegment cand
                  in (Set.disjoint ls segs, cand, ls, Set.intersection (Set.mapMonotonic (+ below) ls) segs)

support :: Brick -> Brick -> Bool
support (x,y) (a,b) = not $ Set.disjoint sup top
  where
    sup = Set.fromList $ lineSegment x y
    top = Set.fromList $ map (below +) $ lineSegment a b

collide :: Set Brick -> Brick -> Bool
collide bs (a,b) = any (\(x,y) -> not $ Set.disjoint (Set.fromList (lineSegment a b)) (Set.fromList $ lineSegment x y)) bs

mkDag :: Map Brick (Set (V3 Int)) -> Map Brick [Brick]
mkDag mp = dag
  where
    dag = Map.unions $ do
      b <- Map.keys mp
      let ls = uncurry lineSegment b
      pure $ Map.singleton b (nub $ mapMaybe (`Map.lookup` supportMap) ls)
    supportMap :: Map (V3 Int) Brick
    supportMap = Map.unions $ do
      (k, v) <- Map.assocs mp
      v3 <- Set.toList v
      pure $ Map.singleton v3 k

disintegrate :: Map Brick [Brick] -> Map Brick [Brick] -> [Brick]
disintegrate supports supportedBy = do
  b <- Map.keys supports
  let s = map (length . (supportedBy Map.!)) $ supports Map.! b
  guard $ all (> 1) s
  pure b

chainReaction :: Map Brick [Brick] -> Map Brick [Brick] -> Int
chainReaction supports supportedBy = sum $ map (start supportedBy . pure) $ Map.keys supports 
  where
    start supBy bricks = if null sups
                            then 0
                            else length sups + start supBy' sups
      where
        supBy' = Map.map (filter (`notElem` bricks)) supBy
        sups = nub $ concatMap (filter (null . (supBy' Map.!)) . (supports Map.!)) bricks

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day22.in"
  let mp = fallFast input
  let supports = mkDag mp
  let supportedBy = invertMap supports
  print $ length $ disintegrate supports supportedBy
  print $ chainReaction supports supportedBy


parseInput = Set.fromList . map f . lines
  where
    f = g . map allNums . splitOn "~"
    g [[a,b,c], [x,y,z]] = (V3 a b c, V3 x y z)
