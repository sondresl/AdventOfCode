module Day09 where

import Lib (bfs, parseAsciiMap, neighbours4)
import Linear (V2(..))
import Data.Maybe (mapMaybe)
import Data.List.Extra (sortBy)
import Control.Applicative (liftA2)
import Data.Map (Map)
import qualified Data.Map as Map

lowPoints :: Map (V2 Int) Int -> [(V2 Int, Int)]
lowPoints input = Map.toList $ Map.filterWithKey lowPoint input
  where lowPoint k v = all (v <) . mapMaybe (`Map.lookup` input) $ neighbours4 k

basin :: Map (V2 Int) Int -> (V2 Int, Int) -> Int
basin input (pos, val) = length $ bfs [pos] f
  where
    f = filter ((/= 9) . flip (Map.findWithDefault 9) input) . neighbours4

main :: IO ()
main = do
  input <- parseAsciiMap (Just . read . pure) <$> readFile "../data/day09.in"
  let lps = lowPoints input
  print . sum . map ((+1) . snd) $ lps
  print . product . take 3 . sortBy (flip compare) $ map (basin input) lps

-- 545
-- 950600
