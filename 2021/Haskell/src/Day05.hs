module Day05 where

import Lib (count, lineSegment, (.:), freqs)
import Linear (V2(..))
import Control.Applicative (liftA2)
import Text.ParserCombinators.Parsec (parse, many1, digit, char, string)
import qualified Data.Map as Map

type Line = (V2 Int, V2 Int)

run :: (Line -> Bool) -> [Line] -> Int
run f = count (> 1) . freqs . concatMap (uncurry lineSegment) . filter f

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day05.in"
  print $ run (uncurry $ foldr1 (||) .: liftA2 (==)) input
  print $ run (const True) input

parseInput :: String -> [(V2 Int, V2 Int)]
parseInput = either (error . show) id . traverse (parse p "") . lines
  where
    tuple = V2 <$> (num <* char ',') <*> num
    num = read <$> many1 digit
    p = (,) <$> (tuple <* string " -> ") <*> tuple

-- 6666
-- 19081
