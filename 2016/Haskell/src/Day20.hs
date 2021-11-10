module Day20 where

import Control.Lens
import Data.List.Extra (sort)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Lib
import Text.ParserCombinators.Parsec

parseInput :: String -> [(Int, Int)]
parseInput = either (error . show) id . traverse (parse nums "") . lines
  where
    nums = do
      from <- read @Int <$> many1 digit
      char '-'
      to <- read @Int <$> many1 digit
      pure (from, to)

type Pair = (Int, Int)

merge :: [Pair] -> [Pair]
merge [] = []
merge [x] = [x]
merge ((x, y) : (a, b) : rest)
  | y > b = merge $ (x, y) : rest
  | y > a = merge $ (x, b) : rest
  | otherwise = (x, y) : merge ((a, b) : rest)

part1 :: [Pair] -> Maybe Int
part1 = fmap ((+ 1) . snd) . listToMaybe . merge . sort

part2 :: [Pair] -> Int
part2 =
  sum
    . map (uncurry diff)
    . (zip <*> tail)
    . merge
    . ((0, 0) :)
    . (++ [(4294967295, 0)])
    . sort
  where
    diff (_, x) (y, _) = max (y - x - 1) 0

main :: IO ()
main = do
  let run file = do
        input <- parseInput <$> readFile file
        print $ part1 input
        print $ part2 input

  run "../data/day20.in"

-- Just 753115
-- 113
