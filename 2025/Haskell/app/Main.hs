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

run :: String -> IO () -> IO ()
run day f = putStrLn ("\n\n" <> day <> "\n") >> f

main :: IO ()
main = do
  run "Day01:" Day01.main
  -- run "Day02:" Day02.main
  -- run "Day03:" Day03.main
  -- run "Day04:" Day04.main
  -- run "Day05:" Day05.main
  -- run "Day06:" Day06.main
  -- run "Day07:" Day07.main
  -- run "Day08:" Day08.main
  -- run "Day09:" Day09.main
  -- run "Day10:" Day10.main
  -- run "Day11:" Day11.main
  -- run "Day12:" Day12.main
