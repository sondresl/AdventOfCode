module Day16 where

import Lib
import Advent.Coord
import Data.Maybe
import Control.Lens
import Control.Monad
import Data.List.Extra
import Text.ParserCombinators.Parsec hiding (count)

part1 input = undefined

part2 input = undefined

main :: IO ()
main = do

  let run str file = do
        input <- parseInput <$> readFile file
        putStrLn str
        print input

        -- print $ part1 input
        -- print $ part2 input
    
  run "\nTest:\n\n" "../data/test.in"
  -- run "\nActual:\n\n" "../data/day16.in"

parseInput = id