module Day04 where

import Control.Lens
import Lib
import Data.List.Extra
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Data.Ord (comparing)

data Room = Room
  { name :: [String]
  , roomId :: Int
  , checksum :: String
  } deriving Show


-- parseInput :: String -> [String]
parseInput = either (error . show) id . traverse (parse room "") . lines
  where
    room = do
      names <- many1 name
      id <- read <$> many1 digit
      char '['
      check <- many1 letter
      char ']'
      pure $ Room names id check
    name = many1 letter <* oneOf "-"


validRoom :: Room -> Bool
validRoom (Room names sector check) = counts == check
  where
    counts = map snd . take 5 . sortBy f . map swap . Map.toList . freqs $ concat names
    f (n, a) (m, b) = compare m n <> compare a b

part1 :: [Room] -> Int
part1 = sum . map roomId . filter validRoom

part2 :: [Room] -> Maybe Int
part2 = fmap roomId . find (any (isInfixOf "northpole") . name) . map decrypt . filter validRoom
  where
    letters c = dropWhile (/= c) $ cycle ['a' .. 'z']
    decrypt rr@(Room names sector _) = rr { name = map (map f) names }
      where
        f c = (!! sector) $ letters c

main :: IO ()
main = do
  let run file = do
        input <- parseInput <$> readFile file
        putStrLn ("\nInput file: " ++ show file ++ "\n")
        print $ part1 input
        print $ part2 input

  run "../data/day04.in"

-- 409147
-- Just 991
