module Day23 where

import Lib (tuple, combinations)
import Control.Monad (guard)
import Data.List.Extra (splitOn, maximumOn, sort, intercalate, unfoldr, tails)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (toList)

grouped :: Int -> Map String [String] -> Set (Set String)
grouped n mp = foldr Set.union Set.empty . unfoldr f $ Map.assocs mp
  where
    f [] = Nothing
    f ((k,v):rest) = Just (combos, rest)
      where
        combos = foldr Set.insert Set.empty $ do
          xs <- combinations n v
          guard $ and $ do
            (a : as) <- tails xs
            b <- as
            pure $ b `elem` (mp Map.! a)
          pure $ Set.fromList (k : xs)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day23.in"
  print $ length $ filter (any (('t' ==) . head)) . toList $ grouped 2 input
  putStrLn $ intercalate "," 
           . sort . toList 
           . maximumOn Set.size . toList 
           $ grouped 12 input

parseInput :: String -> Map String [String]
parseInput = Map.unionsWith (<>) .  map (f . tuple . splitOn "-") . lines
  where
    f (a,b) = Map.fromList [(a, [b]), (b, [a])]

-- 1175
-- bw,dr,du,ha,mm,ov,pj,qh,tz,uv,vq,wq,xw
