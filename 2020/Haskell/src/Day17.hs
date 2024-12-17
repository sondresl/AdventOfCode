{-# LANGUAGE FlexibleContexts #-}
module Day17 where

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V2 (..), V3 (..), V4 (..))
import Lib (parseAsciiMap, count, neighbours, conway)

parseInput :: String -> Set (V2 Int)
parseInput = Map.keysSet . parseAsciiMap f
  where
    f '#' = Just ()
    f _ = Nothing

solve :: (Ord (t a), Traversable t, Applicative t, Num a) => Set (t a) -> Int
solve = Set.size . (!! 6) . iterate (conway neighbours f)
  where
    f :: (Bool, Int) -> Bool
    f (True, 2) = True
    f (True, 3) = True
    f (False, 3) = True
    f _ = False

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/day17.in"
    let v2tov3 (V2 x y) = V3 x y 0
        v2tov4 (V2 x y) = V4 x y 0 0
    print $ solve (Set.map v2tov3 input)
    print $ solve (Set.map v2tov4 input)

-- 240
-- 1180
