module Day10 where

import Lib
import Advent.Coord
import Advent.Search
import Data.Maybe
import Control.Lens hiding ((.>))
import Control.Monad
import Control.Monad.State
import Data.List.Extra
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.RawString.QQ
import Text.ParserCombinators.Parsec hiding (count)
import Debug.Trace
import Data.SBV hiding (solve)
import Data.Foldable

data Machine = Machine
  { lights :: IntMap Bool
  , target :: IntMap Bool
  , buttons :: [Set Int]
  , joltage :: IntMap Int
  } deriving Show

go :: Machine -> Int
go (Machine ls ta bu _) = head $ map snd $ filter ((== ta) . fst) path
  where
    path = bfsOn fst [(ls, 0)] nexts
    nexts (state, i) = map (,succ i) $ map (\bs -> Map.mapWithKey (\k v -> if k `Set.member` bs then not v else v) state) bu

solve :: Machine -> IO OptimizeResult
solve = optimize Independent . solve
    where
      solve (Machine _ _ bu jo) = do
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
  results <- mapM solve ms
  let res = mapMaybe extract results
  print $ sum res
    where
      extract :: OptimizeResult -> Maybe Integer
      extract (IndependentResult [(_, result)]) = getModelValue "sum" result

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day10.in"
  print . sum $ map go input
  runSolve input

parseInput :: String -> [Machine]
parseInput = map (p . words) . lines
  where
    p input = let ls = lights input in Machine (fmap (const False) ls) ls (buttons input) (joltage input)
    joltage = Map.fromList . zip [0..] . allNums . last
    buttons = map (Set.fromList . allNums) . tail . init
    lights = Map.fromList . zip [0..] . map f . tail . init . head
    f = \case 
      '.' -> False
      '#' -> True

-- 401
-- 15017
