module Day02 where

import Lib (combineWith)
import Control.Monad (guard)
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Linear (V3 (..))
import Text.ParserCombinators.Parsec
  (alphaNum, char, digit, many1, parse, sepBy, space, string)

type RGB = V3 Int

part1 :: [(Int, [RGB])] -> Int
part1 = sum . mapMaybe possible
  where
    test (V3 r g b) = r <= 12 && g <= 13 && b <= 14
    possible (cId, xs) = guard (all test xs) $> cId

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day02.in"
  print $ part1 input
  print $ sum $ map (product . combineWith max . snd) input

parseInput :: String -> [(Int, [RGB])]
parseInput = either (error . show) id . traverse (parse p "") . lines
  where
    cube = do
      cnt <- read <$> (space *> many1 digit <* space)
      colour <- many1 alphaNum
      case colour of
        "red" -> pure (V3 cnt 0 0)
        "green" -> pure (V3 0 cnt 0)
        "blue" -> pure (V3 0 0 cnt)
    cubes = combineWith (+) <$> sepBy cube (string ",")
    p = do
      gameId <- read <$> (string "Game " *> many1 digit <* string ":")
      cs <- sepBy cubes (char ';')
      pure (gameId, cs)

-- 2377
-- 71220
