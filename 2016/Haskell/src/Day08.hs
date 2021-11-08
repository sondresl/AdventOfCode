module Day08 where

import Control.Lens
import Lib
import Data.List.Extra
import Text.ParserCombinators.Parsec
import Data.Set (Set)
import qualified Data.Set as Set
import Linear

type Screen = Set Point

data Instruction
  = Rect Int Int
  | Col Int Int
  | Row Int Int
  deriving Show

run :: Screen -> Instruction -> Screen
run screen (Rect x y) = 
  let new = Set.fromList [ V2 x y | x <- [0 .. x - 1], y <- [0 .. y - 1] ] 
   in screen `Set.union` new
run screen (Col col amount) = 
  let f (V2 x y) = 
        if col == x then V2 x ((y + amount) `mod` 6) else V2 x y
   in Set.map f screen
run screen (Row row amount) =
  let f (V2 x y) = 
        if row == y then V2 ((x + amount) `mod` 50) y else V2 x y
   in Set.map f screen


part1 :: [Instruction] -> Int
part1 = Set.size . foldl run Set.empty

part2 :: [Instruction] -> String
part2 = display toTup (0,0,49,5) . foldl run Set.empty
  where
    toTup (V2 x y) = ((y, x), True)

main :: IO ()
main = do
  let run file = do
        input <- parseInput <$> readFile file
        putStrLn ("\nInput file: " ++ show file ++ "\n")
        print $ part1 input
        putStrLn $ part2 input

  run "../data/day08.in"

--
--

parseInput :: String -> [Instruction]
parseInput = either (error . show) id . traverse (parse instruction "") . lines
  where
    instruction :: Parser Instruction
    instruction = choice
      [ try parseRect
      , try parseColMore
      , try parseCol
      , try parseRowMore
      , parseRow
      ]
    parseRect = do
      string "rect "
      x <- read <$> many1 digit
      string "x"
      y <- read <$> many1 digit
      pure $ Rect x y
    parseColMore = do
      string "rotate column x="
      x <- read <$> many1 digit
      string " by "
      y <- read <$> many1 digit
      pure $ Col x y
    parseCol = do
      string "rotate column x="
      x <- read <$> many1 digit
      pure $ Col x 1
    parseRowMore = do
      string "rotate row y="
      x <- read <$> many1 digit
      string " by "
      y <- read <$> many1 digit
      pure $ Row x y
    parseRow = do
      string "rotate row y="
      x <- read <$> many1 digit
      pure $ Row x 1

