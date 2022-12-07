module Day07 where

import Lib
import Data.Maybe
import Control.Monad.State
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Either
import Data.Bifunctor

import Debug.Trace

data File = File
  { name :: String
  , size :: Int
  } deriving (Show, Eq, Ord)

-- Just a string means it is a folder
type System = Map [String] [Either String File]
type Path = [String]

parseInput :: String -> State (System, Path) ()
parseInput str = case splitOn " " str of
  ["$", "cd", "/"] -> modify (second $ const ["/"])
  ["$", "cd", ".."] -> modify $ second init
  ["$", "cd", dir] -> do
    path <- snd <$> get
    modify $ second (<> [dir])
    modify $ first (Map.insertWith (<>) path [Left dir])
  ["$", "ls"] -> pure ()   -- Noise
  ["dir", name] -> pure () -- Noise
  [read -> size, name] -> do
    path <- snd <$> get
    modify (\(a,b) -> (Map.insertWith (<>) path [Right (File name size)] a, b))

data Tree = Tree String (Either Int [Tree])
  deriving (Show, Ord, Eq)

mkTree :: System -> Tree
mkTree m = go ["/"]
  where
    go path = 
      let files = fromMaybe [] $ map (\(File name size) -> Tree name (Left size)) . rights <$> Map.lookup path m
          folders = fromMaybe [] $ map (\name -> go (path <> [name])) . lefts <$> Map.lookup path m
       in Tree (last path) (Right $ files <> folders)

part1 :: Tree -> Either Int (Int, [Int])
part1 (Tree name (Left i)) = Left i
part1 (Tree name (Right xs)) = let (tot, ch) = children
                                in Right (tot, tot : ch)
  where
    children = foldl f (0,[]) $ map part1 xs
    f (a,b) (Left i) = (a + i, b)
    f (a,b) (Right (tot, xs)) = (a + tot, b <> xs)

part2 input = undefined

main :: IO ()
main = do
  input <- lines <$> readFile "../data/day07.in"
  let initial = (Map.singleton ["/"] [], []) :: (System, Path)
  let res = fst $ execState (traverse parseInput input) initial 
  let res' = part1 . mkTree $ res
  print $ fmap (sum . filter (<= 100000) . snd) res'
  let Right total = fmap fst res'
  let diff = 30000000 - (70000000 - total)
  print $ fmap (minimum . filter (>= diff) . snd) res'

-- Right 2104783
-- Right 5883165
