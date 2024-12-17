module Day11 where

import Lib (freqs)
import Data.List.Extra (splitOn, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec (string, many1, digit, parse, newline, sepBy, oneOf, try, char, (<|>))

data Monkey = Monkey
  { number :: Int
  , items :: [Int]
  , modOp :: Int -> Int
  , op :: Int -> Int
  , next :: Int -> Int
  }

type Factors = Map Int Int

runGift :: (Int -> Int) -> Map Int Monkey -> (Int, Factors) -> (Int, Factors)
runGift f mp (mnk, worry) = (to, worry')
  where
    current = mp Map.! mnk
    worry' = Map.mapWithKey (\k i -> let (Monkey _ _ modOp _ _) = mp Map.! k in modOp (f $ op current i)) worry
    to = next current (worry' Map.! mnk)

-- Round number increases when the monkey ID goes down, since
-- then the number will only be evaluated the next time around.
countRounds :: Int -> [(Int, Factors)] -> Map Int Int
countRounds rs = freqs . f 0 . (zip <*> tail)
  where 
    f round (((a,b), (x,y)):xs)
      | round == rs = []
      | x < a = a : f (succ round) xs
      | otherwise = a : f round xs

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day11.in"
  let gifts = concatMap (\(Monkey num xs op modOp test) -> map (num,) xs) $ Map.elems input
  let starts (num, val) = (num,) $ Map.map (\(Monkey _ _ modOp _ _) -> modOp val) input
      part2 = Map.unionsWith (+) $ map (countRounds 10000 . iterate (runGift id        input) . starts) gifts
      score = product . take 2 . reverse . sort . Map.elems
  print $ score part2

parseInput :: String -> Map Int Monkey
parseInput = Map.fromList . map (\x -> (number x, x)) . either (error . show) id . traverse (parse p "") . splitOn "\n\n"
  where
    p = do
      num <- read <$> (string "Monkey " *> many1 digit <* string ":" <* newline)
      items <- string "  Starting items: " *> sepBy (many1 digit) (string ", ") <* newline
      op <- string "  Operation: new = old " *> oneOf "*+"
      v <- try (char ' ' *> many1 digit <* newline) <|> (char ' ' *> string "old" <* newline)
      test <- read <$> (string "  Test: divisible by " *> many1 digit <* newline)
      true <- read <$> (string "    If true: throw to monkey " *> many1 digit <* newline)
      false <- read <$> (string "    If false: throw to monkey " *> many1 digit)
      let modOp x = x `mod` test
          opF = case (v, op) of 
                  ("old", _) -> (^2)
                  (n, '+') -> (read v +)
                  (n, '*') -> (read v *)
          nextF y = if y == 0 then true else false
      pure $ Monkey num (map read items) modOp opF nextF

-- 69918
-- 19573408701
