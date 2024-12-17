module Day10 where

import Lib (middle, (.:))
import Data.List.Extra (sort)
import Control.Monad (foldM)
import Data.Either (partitionEithers)

par :: String -> Char -> Either Char String
par [] c = Right [c]
par stack c | c `elem` "([{<" = Right (c : stack)
par (x:xs) c
  | (x,c) `elem` [('(', ')'), ('{', '}'), ('[', ']'), ('<', '>')] = Right xs
  | otherwise = Left c

main :: IO ()
main = do
  input <- lines <$> readFile "../data/day10.in"
  let (errors, remaining) = partitionEithers $ map (foldM par []) input
  print $ sum $ map score1 errors
  print . middle . sort $ map (foldl (\acc new -> acc * 5 + score2 new) 0) remaining
  
score1, score2 :: Char -> Int
score1 x = let Just val = lookup x [(')', 3), (']', 57), ('}', 1197), ('>', 25137)] in val
score2 x = let Just val = lookup x [('(', 1), ('[',  2), ('{',    3), ('<',     4)] in val

-- 411471
-- 3122628974
