module Day05 where

import Lib (count, lineSegment, (.:), freqs)
import Linear (V2(..))
import Control.Applicative (liftA2)
import Text.ParserCombinators.Parsec (parse, many1, digit, char, string)
import qualified Data.Map as Map

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day05.in"
  let run = count (> 1) . freqs . concatMap (uncurry lineSegment)
  print $ run (filter (uncurry $ foldr1 (||) .: liftA2 (==)) input)
  print $ run input

parseInput :: String -> [(V2 Int, V2 Int)]
parseInput = either (error . show) id . traverse (parse line "") . lines
  where
    num = read <$> many1 digit
    tuple = V2 <$> (num <* char ',') <*> num
    line = (,) <$> tuple <* string " -> " <*> tuple

-- 6666
-- 19081
