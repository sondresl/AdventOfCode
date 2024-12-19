module Day19 where

import Lib (count)
import Data.List.Extra (splitOn, isPrefixOf, isInfixOf)
import Data.MemoTrie (memo)

countOptions :: [String] -> String -> Int
countOptions towels design = go design
  where
    limited = filter (`isInfixOf` design) towels
    go = memo $ \case
        [] -> 1
        xs -> sum $ do
          t <- map length $ filter (`isPrefixOf` xs) limited
          pure $ go (drop t xs)

main :: IO ()
main = do
  (towels, designs) <- parseInput <$> readFile "../data/day19.in"
  let result = map (countOptions towels) designs
  print $ count (> 0) result
  print $ sum result

parseInput :: String -> ([String], [String])
parseInput input = (words $ filter (/= ',') top, lines bot)
  where [top, bot] = splitOn "\n\n" input

-- 306
-- 604622004681855
