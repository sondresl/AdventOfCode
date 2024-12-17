module Day04 where

import Control.Lens ((^..))
import Control.Lens.Regex.Text (match, regex)
import Data.List.Extra (intersect)
import Data.Text (pack, unpack)

winCount :: (Int, [Int], [Int]) -> Int
winCount (cardId, winners, numbers) = length $ intersect winners numbers

score :: Int -> Int
score = ((0 : iterate (* 2) 1) !!)

part2 :: [(Int, [Int], [Int])] -> Integer
part2 input = sum $ map fst $ go cnts
  where
    cnts :: [(Integer, Int)]
    cnts = map ((1,) . winCount) input
    go [] = []
    go ((c, wins) : xs) = (c, wins) : go (map (\(c', x) -> (c' + c, x)) (take wins xs) <> drop wins xs)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day04.in"
  print $ sum $ map (score . winCount) input
  print $ part2 input

parseInput :: String -> [(Int, [Int], [Int])]
parseInput = map (grp . map (read . unpack) . f . pack) . lines
  where
    f txt = txt ^.. [regex|\d+|] . match
    grp xs = (head xs, take 10 (tail xs), drop 11 xs)

-- 26914
-- 13080971
