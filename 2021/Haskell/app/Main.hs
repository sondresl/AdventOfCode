module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25

run :: String -> IO () -> IO ()
run day f = putStrLn ("\n\n" <> day <> "\n") >> f

main :: IO ()
main = do
  -- run "Day01:" Day01.main
  -- run "Day02:" Day02.main
  -- run "Day03:" Day03.main
  -- run "Day04:" Day04.main
  -- run "Day05:" Day05.main
  -- run "Day06:" Day06.main
  -- run "Day07:" Day07.main
  -- run "Day08:" Day08.main
  -- run "Day09:" Day09.main
  -- run "Day10:" Day10.main
  run "Day11:" Day11.main
