module Day11 where

import Lib (freqs)
import Data.Ord (Down(Down))
import Data.List.Extra (splitOn, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec (string, many1, digit, parse, newline, sepBy, oneOf, try, char, (<|>))

type Monkey =
  ( Int -> Int -- Increase worry level
  , Int -> Int -- Test on worry level, return monkey throwing to
  )

runGift
  :: (Int -> Int)   -- Calming function, keeping the worry level in check
  -> Map Int Monkey -- Monkey id to functions
  -> (Int, Int)     -- (current monkey ID, worry level)
  -> (Int, Int)     -- New of ^^
runGift calmF mp (mnk, worry) = ((,) =<< next) $ calmF (op worry)
  where (op, next) = mp Map.! mnk

countRounds
  :: Int          -- Total number of rounds we want to play
  -> [(Int, Int)] -- Every played step of monkey and worry level
  -> Map Int Int  -- Number of inspections by monkey
countRounds rs = freqs . f 0 . (zip <*> tail)
  where
    f round [] = error "countRounds should always terminate"
    f round (((a,b), (x,y)):xs)
      | round == rs = []
      | x < a = a : f (succ round) xs
      | otherwise = a : f round xs

main :: IO ()
main = do
  (Map.fromList -> input, concat -> gifts, product -> mods) <- unzip3 . parseInput <$> readFile "../data/day11.in"
  let run n f = Map.unionsWith (+) $ map (countRounds n . iterate (runGift f input)) gifts
      score = product . take 2 . sortOn Down . Map.elems
  print . score $ run    20 (`div`    3)
  print . score $ run 10000 (`mod` mods)

parseInput :: String -> [((Int, Monkey), [(Int, Int)], Int)]
parseInput = either (error . show) id . traverse (parse p "") . splitOn "\n\n"
  where
    p = do
      num   <- read <$> (string "Monkey " *> many1 digit <* string ":" <* newline)
      items <- string "  Starting items: " *> sepBy (many1 digit) (string ", ") <* newline
      op    <- string "  Operation: new = old " *> oneOf "*+"
      value <- try (char ' ' *> many1 digit <* newline) <|> (char ' ' *> string "old" <* newline)
      test  <- read <$> (string "  Test: divisible by " *> many1 digit <* newline)
      true  <- read <$> (string "    If true: throw to monkey " *> many1 digit <* newline)
      false <- read <$> (string "    If false: throw to monkey " *> many1 digit)
      let opF = case (value, op) of
                  ("old", '*') -> (^2)
                  (    n, '*') -> (read value *)
                  (    n, '+') -> (read value +)
                  _ -> error "opF"
          nextF y = if y `mod` test == 0 then true else false
      pure ((num, (opF, nextF)), map ((num,) . read) items, test)

-- 69918
-- 19573408701
