{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Extra
import Control.Lens

findLoop :: Ord a => [a] -> (Int, Int) -- first loop, size of loop, shift
findLoop (x:xs) = go (Map.singleton x 0) 1 xs
  where
    go !seen !i (w:ws) = case Map.lookup w seen of
                       Nothing -> go (Map.insert w i seen) (i + 1) ws
                       Just seenI -> (seenI, i - seenI)

run :: IntMap Int -> (Int, Int)
run = findLoop . map (map snd . IMap.toList) . iterate step
  where
    step mp = 
      let (i, v) = maximumOn snd $ IMap.toList mp
          new = IMap.fromListWith (+) . map ((,1) . (`mod` IMap.size mp)) $ [i + 1 .. (i + v)]
      in IMap.unionWith (+) new (IMap.insert i 0 mp)

main :: IO ()
main = do
  input <- IMap.fromList . zip [0..] . map read . words <$> readFile "../data/06.in"
  print . uncurry (+) . run $ input
  print . snd . run $ input

-- 6681
-- 2392
