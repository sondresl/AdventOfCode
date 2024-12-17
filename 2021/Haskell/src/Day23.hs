{-# LANGUAGE MultiWayIf #-}
module Day23 where

import Lib (parseAsciiMap, Point, neighbours4, bfs)
import Linear (V2(..))
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.PQueue.Prio.Min as PQ

data Tile = Space | Amphipod Amphipod
  deriving (Show, Eq, Ord)

data Amphipod = Amber | Bronze | Copper | Desert
  deriving (Show, Eq, Ord)

done :: Int -> Map Point Tile -> Bool
done n mp = 
  all ((== Amphipod Amber) . (mp Map.!)) (v2range (V2 3 2) (V2 3 n)) &&
  all ((== Amphipod Bronze) . (mp Map.!)) (v2range (V2 5 2) (V2 5 n)) &&
  all ((== Amphipod Copper) . (mp Map.!)) (v2range (V2 7 2) (V2 7 n)) &&
  all ((== Amphipod Desert) . (mp Map.!)) (v2range (V2 9 2) (V2 9 n))

v2range :: Point -> Point -> [Point]
v2range from to
  | from > to = []
  | from == to = [to]
  | otherwise = from : v2range (from + V2 0 1) to

amphCost :: Tile -> Int
amphCost (Amphipod Amber)  = 1
amphCost (Amphipod Bronze) = 10
amphCost (Amphipod Copper) = 100
amphCost (Amphipod Desert) = 1000

inCorrectPos :: Amphipod -> Point -> Point -> Point -> Map Point Tile -> Bool
inCorrectPos amph start end curr mp
  | start == end = curr == end
  | curr == start = all (\v2 -> mp Map.! v2 == Amphipod amph) (v2range (start + V2 0 1) end)
  | curr /= start = (mp Map.! start == Space) && inCorrectPos amph (start + V2 0 1) end curr mp

fromHallway :: Map Point Tile -> Int -> Tile -> Point -> Bool
fromHallway mp n (Amphipod amph) pos = pos `elem` lst && all (\v2 -> mp Map.! v2 == Amphipod amph) (v2range (pos + V2 0 1) (V2 x n))
  where
    (x, lst) = case amph of
            Amber  -> (3, amber)
            Bronze -> (5, bronze)
            Copper -> (7, copper)
            Desert -> (9, desert)

validLocations :: Map Point Tile -> (Point, Tile) -> [(Int, Point)]
validLocations mp (pos, Amphipod amph) =
  let ns' = tail $ bfs [(0, pos)] (\(cost, v2) -> map (cost + 1,) . filter ((Just Space ==) . (`Map.lookup` mp)) $ neighbours4 v2) snd
   in filter ((`notElem` forbidden) . snd) ns'

solve :: Int -> Map Point Tile -> Int
solve n mp = go Set.empty (PQ.singleton 0 (Nothing, mp))
  where
    go seen q = 
      let ((cost, (prev, next)), q') = PQ.deleteFindMin q
          amphs = do
            (loc, a) <- Map.toList $ Map.filter (/= Space) next
            (c, loc') <- validLocations next (loc, a)
            guard $ case prev of 
                      Nothing -> True
                      Just prev' -> loc /= prev'
            guard . not $ case a of
                            Amphipod Amber  -> inCorrectPos Amber  (V2 3 2) (V2 3 n) loc next
                            Amphipod Bronze -> inCorrectPos Bronze (V2 5 2) (V2 5 n) loc next
                            Amphipod Copper -> inCorrectPos Copper (V2 7 2) (V2 7 n) loc next
                            Amphipod Desert -> inCorrectPos Desert (V2 9 2) (V2 9 n) loc next
            let cost' = cost + (amphCost a * c)
            guard $ if loc `elem` hallway
                       then fromHallway next n a loc'
                       else fromHallway next n a loc' || loc' `elem` hallway
            let next' = Map.insert loc' a $ Map.insert loc Space next
            pure (cost', (Just loc', next'))
          seen' = Set.insert (prev, next) seen
       in if | done n next -> cost
             | (prev, next) `Set.member` seen -> go seen q'
             | otherwise -> go seen' (foldr (uncurry PQ.insert) q' amphs)

hallway, forbidden, amber, bronze, copper, desert :: [V2 Int]
hallway = map (`V2` 1) [1..11]
forbidden = [V2 3 1, V2 5 1, V2 7 1, V2 9 1]
amber  = v2range (V2 3 2) (V2 3 5)
bronze = v2range (V2 5 2) (V2 5 5)
copper = v2range (V2 7 2) (V2 7 5)
desert = v2range (V2 9 2) (V2 9 5)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day23.in"
  let start = Map.toList $ Map.filter (/= Space) input
  print $ solve 3 input
  print $ solve 5 $ part2 <> input

part2 :: Map Point Tile
part2 = Map.fromList [ (V2 3 3, Amphipod Desert), (V2 5 3, Amphipod Copper), (V2 7 3, Amphipod Bronze), (V2 9 3, Amphipod Amber)
                     , (V2 3 4, Amphipod Desert), (V2 5 4, Amphipod Bronze), (V2 7 4, Amphipod Amber),  (V2 9 4, Amphipod Copper)
                     , (V2 3 5, Amphipod Desert), (V2 5 5, Amphipod Copper), (V2 7 5, Amphipod Amber),  (V2 9 5, Amphipod Copper)
                     ]
    
parseInput :: String -> Map Point Tile  
parseInput = parseAsciiMap p
  where
    p '.' = Just Space
    p 'A' = Just (Amphipod Amber)
    p 'B' = Just (Amphipod Bronze)
    p 'C' = Just (Amphipod Copper)
    p 'D' = Just (Amphipod Desert)
    p _ = Nothing

-- 15412
-- 52358
