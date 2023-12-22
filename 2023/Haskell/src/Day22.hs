module Day22 where

import Lib
import Advent.Coord
import Data.Maybe
import Control.Lens hiding (below)
import Control.Monad
import Control.Monad.State
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.RawString.QQ
import Text.ParserCombinators.Parsec hiding (count)
import Linear hiding (trace)

import Debug.Trace

type Brick = (V3 Int, V3 Int)

part1 input = undefined

part2 input = undefined

fall bs = case find (view _1) new of
            Just (_,v,old) -> Set.insert v $ Set.delete old bs
            Nothing -> bs
  where
    new = map f $ Set.toList bs
    f old@(V3 x y z, V3 a b c) = let p = (V3 x y (z - 1), V3 a b (c - 1))
                                     test1 = z > 1 && c > 1
                                     test2 = not $ collide (Set.delete old bs) p
                                  in (test1 && test2, p, old)

fallFast :: Set Brick -> (Set Brick, Map Brick (Set (V3 Int)))
fallFast bs = go bs Set.empty Set.empty Map.empty
  where
    go old new segs supports | Set.null old = (new, supports)
    go old new segs supports = case down lowest of
                   Nothing -> go (Set.delete lowest old) 
                            (Set.insert lowest new) 
                            (segs <> Set.fromList (uncurry lineSegment lowest))
                            (Map.insert lowest (Set.intersection (Set.fromList $ map (+ below) $ uncurry lineSegment lowest) segs) supports)
                   Just (_,p,orig,ls,overlap) -> 
                     go (Set.delete orig old) (Set.insert p new) (segs <> ls) (Map.insert p overlap supports)
      where
        lowest = minimumOn (\(V3 _ _ z, V3 _ _ c) -> min z c) $ Set.toList old
        down d = listToMaybe
               . reverse
               . takeWhile (view _1) 
               . map f
               . takeWhile (\(V3 _ _ z, V3 _ _ c) -> z > 0 && c > 0) 
               $ iterate (bimap (below +) (below +)) d
        f cand = let ls = Set.fromList $ uncurry lineSegment cand
                  in (Set.disjoint ls segs, cand, lowest, ls, Set.intersection (Set.mapMonotonic (+ below) ls) segs)
         -- (not $ collide new cand, cand, lowest)

support (x,y) (a,b) = not $ Set.disjoint sup top
  where
    sup = Set.fromList $ lineSegment x y
    top = Set.fromList $ map (below +) $ lineSegment a b

disintegrate :: Set Brick -> Map Brick (Set (V3 Int)) -> Map (V3 Int) Brick -> [Brick]
disintegrate bs mp supps = nub $ do
  b <- Set.toList bs
  let bSegs = uncurry lineSegment b
  let onTop = mapMaybe (`Map.lookup` supps) bSegs
  let withoutB = Set.difference allSegs (Set.fromList bSegs)
  if null onTop
    then pure b
    else do
      cand <- onTop
      let Just ss = Map.lookup cand mp
          over = Set.mapMonotonic (+ below) ss
      guard $ not $ Set.disjoint over withoutB
      pure b
        
      -- withoutB = Set.difference allSegs $ Set.fromList bSegs
      -- withoutB' = Set.delete bSegs allSegs'
  -- guard True
  -- let bs' = Set.delete b bs
  --     above = Set.filter (support b) bs'
  -- guard $ bs' == fall bs'
  -- pure b
    where
      allSegs = Set.unions $ Set.map (Set.fromList . uncurry lineSegment) bs

collide :: Set Brick -> Brick -> Bool
collide bs (a,b) = any (\(x,y) -> not $ Set.disjoint (Set.fromList (lineSegment a b)) (Set.fromList $ lineSegment x y)) bs

main :: IO ()
main = do

  let run str input = do
        putStrLn str
        let (fa, mp) = fallFast input
            supportMap = Map.unions $ do
              (k, v) <- Map.assocs mp
              map (`Map.singleton` k) $ Set.toList v
        let dis = disintegrate fa mp supportMap
        print $ length dis

        -- print $ part1 input
        -- print $ part2 input
    
  run "\nTest:\n\n" $ parseInput testInput

  input <- parseInput <$> readFile "../data/day22.in"
  run "\nActual:\n\n" input

parseInput = Set.fromList . map f . lines
  where
    f = g . map allNums . splitOn "~"
    g [[a,b,c], [x,y,z]] = (V3 a b c, V3 x y z)

-- parseInput = either (error . show) id . traverse (parse p "") . lines
--   where
--     p = undefined

testInput = [r|1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
|]
