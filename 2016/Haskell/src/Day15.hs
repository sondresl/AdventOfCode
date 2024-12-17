{-# LANGUAGE TypeApplications #-}
module Day15 where

import Control.Lens
import Lib
import Data.List.Extra
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

data Disc = Disc
  { ident :: Int
  , positions :: Int
  , start :: Int
  } deriving Show

initial :: Disc -> (Int, Int)
initial (Disc ident pos start) = (pos - ident `mod` pos - start, pos)

sync :: (Int, Int) -> (Int, Int) -> (Int, Int)
sync (time, period) (time', period') =
  let candidates = dropWhile (< time) $ iterate (+ period') time'
      first = head $ dropWhile ((/= 0) . (`mod` period) . subtract time) candidates
   in (first, lcm period period')

run :: [Disc] -> (Int, Int)
run = foldl sync (0,1) . map initial

part1 :: [Disc] -> Int
part1 = fst . run

part2 :: [Disc] -> Int
part2 = fst . run . (++ [Disc 7 11 0])

main :: IO ()
main = do
  let run file = do
        input <- parseInput <$> readFile file
        putStrLn ("\nInput file: " ++ show file ++ "\n")
        print $ part1 input
        print $ part2 input

  run "../data/day15.in"

-- 121834
-- 3208099

parseInput :: String -> [Disc]
parseInput = either (error . show) id . traverse (parse disc "") . lines
  where
    disc = do
      string "Disc #"
      n <- read @Int <$> many1 digit
      string " has "
      positions <- read @Int <$> many1 digit
      string " positions; at time=0, it is at position "
      position <- read @Int <$> many1 digit
      string "."
      pure $ Disc n positions position

