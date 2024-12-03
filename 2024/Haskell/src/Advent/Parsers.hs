module Advent.Parsers where

import Text.ParserCombinators.Parsec

pNumber :: Parser Int
pNumber = read <$> (try ((:) <$> char '-' <*> many1 digit) <|> many1 digit)

pBetween :: String -> String -> Parser a -> Parser a
pBetween start end = between (string start) (string end)
