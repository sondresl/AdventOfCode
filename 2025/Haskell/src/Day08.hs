module Day08 where

import Lib (allNums, combinations, tuple)
import Data.List.Extra (sortOn, sort, find)
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V3(..), distance, _x)
import Control.Lens (view)
import Data.Ord (Down(Down))

type JunctionBox = V3 Double
type JunctionPair = (JunctionBox, JunctionBox)
type Circuits = Set (Set JunctionBox)

addToSets :: Set (Set JunctionBox) -> Set JunctionBox -> Set (Set JunctionBox)
addToSets sets x = Set.insert (Set.union x (Set.unions xIn)) rest
  where 
    (xIn, rest) = Set.partition (not . Set.disjoint x) sets

solve :: [JunctionBox] -> [(Set JunctionBox, Circuits)]
solve boxes = go (Set.fromList $ map Set.singleton boxes) js
  where

    js :: [Set JunctionBox]
    js = map (\(x,y) -> Set.fromList [x,y]) . sortOn (uncurry distance) $ map tuple $ combinations 2 boxes

    go :: Circuits -> [Set JunctionBox] -> [(Set JunctionBox, Circuits)]
    go _ [] = []
    go state (x:xs) = let new = addToSets state x
                       in (x, new) : go new xs

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day08.in"
  let states = solve input
  print $ product . take 3 . sortOn Down . map Set.size . Set.toList . snd . (!! 999) $ states
  print $ product . Set.map (round . view _x) . fst <$> find ((== 1) . Set.size . snd) states

parseInput :: String -> [JunctionBox]
parseInput = map ((\[x,y,z] -> V3 x y z) . map fromIntegral . allNums) . lines

-- 175500
-- Just 6934702555
