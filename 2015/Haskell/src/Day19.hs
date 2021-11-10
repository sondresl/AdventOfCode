module Day19 where

import Lib ( tuple )
import Data.List.Extra ( intercalate, splitOn )
import qualified Data.Set as Set
import           Data.Set   ( Set )
import Data.Tuple (swap)

parseInput :: String -> (String, [(String, String)])
parseInput input = (x, rules)
  where
    (x : xs) = lines input
    rules = map (tuple . splitOn " => ") xs

replace :: String -> (String, String) -> Set String
replace str (rule, rep) = Set.fromList $ go (head parts) (tail parts)
  where
    parts = splitOn rule str
    go _ [] = []
    go pre (x:xs) = new : go (pre ++ rule ++ x) xs
      where new = pre ++ rep ++ intercalate rule (x:xs)

part1 :: [(String, String)] -> String -> Int
part1 rules str = Set.size . Set.unions $ map (replace str) rules

part2 :: [(String, String)] -> String -> Int
part2 rules = length . takeWhile ((> 1) . length) . iterate (reduce rules)
  where
    reduce rules str = Set.findMax . Set.unions $ map (replace str . swap) rules

main :: IO ()
main = do
  (line, rules) <- parseInput <$> readFile "../data/19.in"
  mapM_ (print . swap) rules
  print $ part1 rules line
  print $ part2 rules line
