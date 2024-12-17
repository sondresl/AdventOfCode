module Day22 where

import Lib (count, allNums, lineSegment, invertMap)
import Advent.Coord (below)
import Data.Maybe (mapMaybe)
import Control.Lens (view, Bifunctor(bimap), Field1(_1))
import Data.List.Extra (nub, minimumOn, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V3(..))

type Brick = (V3 Int, V3 Int)

fall :: Set Brick -> Map Brick (Set (V3 Int))
fall bs = go bs Set.empty Map.empty
  where
    go old segs supports | Set.null old = supports
    go old segs supports = let (_,p,ls,supportedBy) = down lowest
                            in go (Set.delete lowest old) 
                                  (segs <> ls) 
                                  (Map.insert p supportedBy supports)
      where
        lowest = minimumOn (\(V3 _ _ z, V3 _ _ c) -> min z c) $ Set.toList old
        down d = last
               . takeWhile (view _1) 
               . map f
               . takeWhile (\(V3 _ _ z, V3 _ _ c) -> z > 0 && c > 0) 
               $ iterate (bimap (below +) (below +)) d
        f cand = let ls = Set.fromList $ uncurry lineSegment cand
                  in (Set.disjoint ls segs, cand, ls, Set.intersection (Set.mapMonotonic (+ below) ls) segs)

mkDag :: Map Brick (Set (V3 Int)) -> Map Brick [Brick]
mkDag mp = Map.unions 
         . map (Map.singleton <*> nub . mapMaybe (`Map.lookup` supportMap) . uncurry lineSegment) 
         $ Map.keys mp
  where 
    supportMap = foldMap (\(k,v) -> foldMap (`Map.singleton` k) v) $ Map.assocs mp

chainReaction :: Map Brick [Brick] -> Map Brick [Brick] -> [Int]
chainReaction supports supportedBy = map (start supportedBy . pure) $ Map.keys supports 
  where
    start supBy []     = 0
    start supBy bricks = length sups + start supBy' sups
      where
        supBy' = Map.map (filter (`notElem` bricks)) supBy
        sups = nub $ concatMap (filter (null . (supBy' Map.!)) . (supports Map.!)) bricks

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day22.in"
  let mp = fall input
  let supports = mkDag mp
  let supportedBy = invertMap supports
  let res = chainReaction supports supportedBy
  print $ count (== 0) res
  print $ sum res


parseInput :: String -> Set Brick
parseInput = Set.fromList . map f . lines
  where
    f = g . map allNums . splitOn "~"
    g [[a,b,c], [x,y,z]] = (V3 a b c, V3 x y z)

-- 395
-- 64714
