module Day12 where

import Control.Monad (guard)
import Data.List.Extra (delete, splitOn)
import Data.Char (isUpper, isLower)
import Data.Map (Map)
import qualified Data.Map as Map

move :: (String -> Map String Int -> Bool) -> Map String [String] -> Int
move pred mp = length $ go Map.empty "start"
  where
    go seen pos = do
      next <- mp Map.! pos
      guard $ isUpper (head next) || pred next seen
      let seen' = if isLower (head next) then Map.insertWith (+) next 1 seen else seen
      case next of
        "end" -> pure ()
        _ -> go seen' next

main :: IO ()
main = do
  input <- fmap (delete "start") . parseInput <$> readFile "../data/day12.in"
  let part2 x seen = (Map.notMember x seen || not (any (== 2) seen))
  print $ move Map.notMember input
  print $ move part2 input

parseInput :: String -> Map String [String]
parseInput = Map.fromListWith (<>) . concatMap (f . splitOn "-") . lines
  where f [x,y] = [(x, [y]), (y, [x])]

-- 3563
-- 105453
