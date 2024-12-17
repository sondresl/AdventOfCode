module Day03 where

import Lib
import Advent.Coord
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Text.RawString.QQ
import Text.ParserCombinators.Parsec hiding (count)
import Data.Functor (($>))

part1 input = undefined

part2 input = undefined

main :: IO ()
main = do

  let run str input = do
        putStrLn str
        print input

        -- print $ part1 input
        -- print $ part2 input

  run "\nTest:\n\n" $ parseInput testInput

  input <- parseInput <$> readFile "../data/day03.in"
  run "\nActual:\n\n" input

-- parseInput = concat . lines

parseInput :: String -> Int
parseInput = sum . either (error . show) id . parse enabled "" . concat . lines
  where
    enabled = (eof $> []) <|> try disable <|> try mul <|>  (anyChar *> enabled)
    disabled = (eof $> []) <|> try enable <|> (anyChar *> disabled)
    enable = string "do()" *> enabled
    disable = string "don't()" *> disabled
    mul = do
      string "mul("
      x <- read <$> many1 digit <* char ','
      y <- read <$> many1 digit <* char ')'
      ((x * y):) <$> enabled

testInput = [r|xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
|]
