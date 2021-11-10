module Day06 where

import Control.Lens
import Lib
import Data.List.Extra
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

parseInput = lines

part1 :: [String] -> Maybe String
part1 = fmap (map fst) . traverse (maximumVal . freqs) . transpose

part2 :: [String] -> Maybe String
part2 = fmap (map fst) . traverse (minimumVal . freqs) . transpose

main :: IO ()
main = do
  let run file = do
        input <- parseInput <$> readFile file
        putStrLn ("\nInput file: " ++ show file ++ "\n")
        print $ part1 input
        print $ part2 input

  run "../data/day06.in"

-- Just "dzqckwsd"
-- Just "lragovly"
