module Advent.Parsers where

import Text.ParserCombinators.Parsec

pNumber :: Parser Int
pNumber = read <$> (try ((:) <$> char '-' <*> many1 digit) <|> many1 digit)

pBetween :: String -> String -> Parser a -> Parser a
pBetween start end = between (string start) (string end)

pFind :: Parser a -> Parser a
pFind = try . pSkipManyTill anyChar

-- https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/src/Control.Monad.Combinators.html#skipManyTill
pSkipManyTill p end = go
  where
    go = do
      r <- optionMaybe $ try end
      case r of
        Nothing -> p >> go
        Just r -> pure r
