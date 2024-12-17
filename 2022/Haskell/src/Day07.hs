module Day07 where

import Control.Monad.State (State(..), modify, gets, execState)
import Data.List.Extra (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Either (rights, lefts, partitionEithers)
import Data.Bifunctor (first, second, bimap)
import Data.Foldable (toList)
import Data.Tree (Tree(..))

type System = Map [String] [Either String Int]
type Path = [String]

parseInput :: String -> State (System, Path) ()
parseInput str = case splitOn " " str of
  ["$", "cd"] -> modify (second $ const ["/"])
  ["$", "cd", ".."] -> modify $ second tail
  ["$", "cd", dir] -> do
    path <- gets snd
    modify $ second (dir:)
    modify $ first (Map.insertWith (<>) path [Left dir])
  ["$", "ls"] -> pure ()   -- Noise
  ["dir", name] -> pure () -- Noise
  [read -> size, name] -> do
    path <- gets snd
    modify $ first (Map.insertWith (<>) path [Right size]) 
  e -> error (show e)

directories :: System -> Tree Int
directories m = go ["/"]
  where
    go path = Node (fileSum + sum (map rootLabel folders)) folders
      where (folders, fileSum) = bimap (map (\n -> go (n:path))) sum . partitionEithers $ m Map.! path

main :: IO ()
main = do
  input <- lines <$> readFile "../data/day07.in"
  let dirs = toList . directories . fst $ execState (traverse parseInput input) (Map.singleton ["/"] [], [])
  print $ sum $ filter (<= 100000) dirs
  let diff = 30000000 - (70000000 - maximum dirs)
  print $ minimum $ filter (>= diff) dirs

-- 2104783
-- 5883165
