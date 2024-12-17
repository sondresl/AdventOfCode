module Day03 where

import Advent.Parsers (pBetween, pNumber, pFind)
import Text.ParserCombinators.Parsec
    (Parser, parse, sepBy, char, many, option, string, (<|>))
import Data.Functor (($>))

parseInput :: Parser a -> String -> a
parseInput f = either (error . show) id . parse f ""

mul :: Parser Int
mul = pBetween "mul(" ")" ((*) <$> pNumber <* char ',' <*> pNumber)

part1 :: String -> Int
part1 = parseInput (sum <$> many (pFind mul))

part2 :: String -> Int
part2 = parseInput enabled
  where
    enabled  = option 0 . pFind $ 
      string "don't" *> disabled
      <|> (+) <$> mul <*> enabled
    disabled = option 0 . pFind $ string "do()" *> enabled

main :: IO ()
main = do
  input <- readFile "../data/day03.in"
  print $ part1 input
  print $ part2 input

-- 175015740
-- 112272912
