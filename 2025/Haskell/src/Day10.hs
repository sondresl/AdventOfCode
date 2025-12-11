module Day10 where

import Lib (allNums)
import Advent.Search (bfsOn)
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.SBV hiding (solve)
import Data.Foldable (for_)
import Data.Bool (bool)

data Machine = Machine
  { lights :: IntMap Bool
  , buttons :: [Set Int]
  , joltage :: IntMap Int
  } deriving Show

part1 :: Machine -> Int
part1 (Machine ls bu _) = head $ map snd $ filter ((== ls) . fst) path
  where
    path = bfsOn fst [(False <$ ls, 0)] nexts
    nexts (state, i) = map (,succ i) $ map (\bs -> Map.mapWithKey (\k v -> bool v (not v) (k `Set.member` bs)) state) bu

solve :: Machine -> IO OptimizeResult
solve = optimize Independent . solve
    where
      solve (Machine _ bu jo) = do
        total <- sInteger "sum"
        constrain $ total .> 0
        ints <- sIntegers $ map show bu
        for_ ints $ \i -> constrain $ i .>= literal 0
        for_ (Map.toList jo) $ \(ix, jolt) -> do
          let is = map snd $ filter (\(bSet, _) -> Set.member ix bSet) (zip bu ints)
          constrain $ sum is .== literal (fromIntegral jolt)
        constrain $ total .== sum ints
        minimize "Presses" total

runSolve :: [Machine] -> IO ()
runSolve ms = do
  results <- traverse solve ms
  let Just res = traverse extract results
  print $ sum res
    where
      extract :: OptimizeResult -> Maybe Integer
      extract (IndependentResult [(_, result)]) = getModelValue "sum" result

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day10.in"
  print . sum $ map part1 input
  runSolve input

parseInput :: String -> [Machine]
parseInput = map (p . words) . lines
  where
    p input = Machine lights buttons joltage
      where
        joltage = Map.fromList . zip [0..] . allNums $ last input
        buttons = map (Set.fromList . allNums) . tail $ init input
        lights = Map.fromList . zip [0..] . map (== '#') . tail . init . head $ input

-- 401
-- 15017
