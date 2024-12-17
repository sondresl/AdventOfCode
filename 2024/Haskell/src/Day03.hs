module Day03 where

import Advent.Parsers (pNumber, pBetween)
import Text.ParserCombinators.Parsec
    (anyChar, char, string, option, (<|>), parse, try, Parser, sepBy, between)
import Data.Functor (($>))

parseInput :: Parser [Int] -> String -> Int
parseInput f = sum . either (error . show) id . parse f "" . concat . lines

mul :: Parser [Int] -> Parser [Int]
mul f = do
  [x, y] <- pBetween "mul(" ")" (pNumber `sepBy` char ',')
  ((x * y):) <$> f

-- Helpers
enable, disable, disabled :: Parser [Int]
enable   = string "do()"    *> part2
disable  = string "don't()" *> disabled
disabled = option [] $ try enable  <|> (anyChar *> disabled)

part1 :: Parser [Int]
part1 = option [] $ try (mul part1) <|> (anyChar *> part1)

part2 :: Parser [Int]
part2 = option [] $ try disable <|> try (mul part2) <|> (anyChar *> part2)

main :: IO ()
main = do
  input <- readFile "../data/day03.in"
  print $ parseInput part1 input
  print $ parseInput part2 input

-- 175015740
-- 112272912
