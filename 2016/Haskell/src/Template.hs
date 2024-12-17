module Template where

import Control.Lens
import Lib
import Data.List.Extra
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

parseInput = lines

part1 = undefined

part2 = undefined

main :: IO ()
main = do
  let run file = do
        input <- parseInput <$> readFile file
        putStrLn ("\nInput file: " ++ show file ++ "\n")
        print input
        -- print $ part1 input
        -- print $ part2 input

  run "../data/test"
  -- run "../data/day05.in"

-- 409147
-- Just 991
