{-# LANGUAGE LambdaCase #-}
module Day10 where

import Data.List.Extra (sort)

paren :: String -> (Int, String)
paren = go []
  where
    go [] [] = (0, [])
    go [] (r:rs) = go [r] rs
    go stack [] = (0, stack)
    go stack@(s:ss) (x:xs) 
     | x `elem` "([{<" = go (x : stack) xs
     | (s,x) `elem` [('(', ')'), ('{', '}'), ('[', ']'), ('<', '>')] = go ss xs
     | otherwise = (score x, [])

score = \case
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137

score' = \case 
  '(' -> 1
  '[' -> 2
  '{' -> 3
  '<' -> 4

main :: IO ()
main = do
  input <- lines <$> readFile "../data/day10.in"
  let res = map paren input
  print . sum $ map fst res

  let remaining = foldl (\acc new -> acc * 5 + score' new) 0
  print . (\xs -> sort xs !! (length xs `div` 2)) 
        . map (remaining . snd) 
        $ filter ((== 0) . fst) res

-- 411471
-- 3122628974
