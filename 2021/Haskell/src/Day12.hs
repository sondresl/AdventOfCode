module Day12 where

import Control.Monad (guard)
import Data.List.Extra (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map

move :: (String -> Map String Int -> Bool) -> Map String [String] -> Int
move pred mp = length $ go Map.empty "start"
  where
    isSmallCave = all (`elem` "abcdefghijklmnopqrstuvwxyz")
    go seen pos = do
      next <- filter (/= "start") $ mp Map.! pos
      guard $ not (isSmallCave next) || pred next seen
      if next == "end"
         then pure ()
         else go (if isSmallCave next then Map.insertWith (+) next 1 seen else seen) next

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day12.in"
  let part2 x seen = (Map.notMember x seen || not (any (== 2) seen))
  print $ move Map.notMember input
  print $ move part2 input

parseInput :: String -> Map String [String]
parseInput = Map.unionsWith (<>) . map (f . splitOn "-") . lines
  where
    f [x,y] = Map.fromList [(x, [y]), (y, [x])]

-- 3563
-- 105453
