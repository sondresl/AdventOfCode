module Day05 where

import Lib (count, lineSegment, (.:), freqs)
import Linear (V2(..))
import Data.List (partition)
import Control.Applicative (liftA2)
import Text.ParserCombinators.Parsec (parse, many1, digit, char, string)
import qualified Data.Map as Map

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day05.in"
  let run = freqs . concatMap (uncurry lineSegment)
      (straight, diags) = partition (uncurry $ foldr1 (||) .: liftA2 (==)) input
      hor = run straight
      all = Map.unionWith (+) hor $ run diags
  print $ count (> 1) hor
  print $ count (> 1) all

parseInput :: String -> [(V2 Int, V2 Int)]
parseInput = either (error . show) id . traverse (parse line "") . lines
  where
    num = read <$> many1 digit
    tuple = V2 <$> (num <* char ',') <*> num
    line = (,) <$> tuple <* string " -> " <*> tuple

-- 6666
-- 19081
