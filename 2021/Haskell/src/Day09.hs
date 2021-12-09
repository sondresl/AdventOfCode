module Day09 where

import Lib (bfs, parseAsciiMap, neighbours4)
import Linear (V2(..))
import Data.Maybe (mapMaybe)
import Data.List.Extra (sortBy)
import Control.Applicative (liftA2)
import Data.Map (Map)
import qualified Data.Map as Map

basin :: Map (V2 Int) Int -> (V2 Int, Int) -> Int
basin input (pos, val) = length $ bfs [pos] f
  where
    f = filter (liftA2 (&&) (val <) (/= 9) . flip (Map.findWithDefault 0) input) . neighbours4

main :: IO ()
main = do
  input <- parseAsciiMap (Just . read . pure) <$> readFile "../data/day09.in"
  let lowPoint k v = all (v <) . mapMaybe (`Map.lookup` input) $ neighbours4 k
      lowPoints = Map.toList $ Map.filterWithKey lowPoint input

  print . sum . map ((+1) . snd) $ lowPoints
  print . product . take 3 . sortBy (flip compare) $ map (basin input) lowPoints

-- 545
-- 950600
