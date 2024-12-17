module Day06 where

import Lib
import Advent.Coord
import Advent.Search
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.RawString.QQ
import Text.ParserCombinators.Parsec hiding (count)
import Linear

data Tiles = Floor | Block | Start
  deriving (Show, Eq, Ord)

type Guard = (Coord, Dir)

type Lab = Map Coord Tiles

step :: Lab -> Guard -> Maybe Guard
step lab (pos, dir) = 
    case Map.lookup (pos + dir) lab of
      Nothing -> Nothing
      Just Floor -> Just (pos + dir, dir)
      Just Block -> Just (pos, turnLeft dir)

tryLoop :: Coord -> Lab -> [Guard] -> Maybe Coord
tryLoop startPos lab toThisPoint = 
  if notInPathSoFar && notStart && notBlocked && isJust (firstRepeat path)
    then Just (pos + dir)
    else Nothing
  where 
    (pos, dir) = last toThisPoint
    notInPathSoFar = pos + dir `notElem` map fst (init toThisPoint)
    notStart = pos + dir /= startPos
    notBlocked = Map.lookup (pos + dir) lab == Just Floor
    lab' = Map.insert (pos + dir) Block lab
    path = iterateMaybe (step lab') (pos, dir)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day06.in"
  let (lab, guard) = input
  let path = iterateMaybe (step lab) guard
  print $ length . nub . map fst $ path
  print $ length . nub $ mapMaybe (tryLoop (fst guard) lab) (tail $ inits path)

parseInput :: String -> (Lab, Guard)
parseInput str = (Map.insert start Floor input, (start, down))
  where
    input = parseAsciiMap f str
    f '#' = Just Block
    f '.' = Just Floor
    f '^' = Just Start
    start = head [ pos | (pos, Start) <- Map.assocs input ]

-- 4789
-- 1304
