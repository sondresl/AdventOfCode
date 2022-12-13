module Day13 where

import Lib (tuple)
import Data.List.Extra (splitOn, elemIndex, sort)
import Text.ParserCombinators.Parsec (between, char, sepBy, many1, digit, parse, (<|>))

data Tree = Node [Tree] | Leaf Int
  deriving (Show, Eq)

instance Ord Tree where
  compare (Leaf l ) (Leaf r ) = l `compare` r
  compare (Node ls) (Node rs) = compare ls rs
  compare (Leaf l ) (Node rs) = compare (Node [Leaf l]) (Node rs)
  compare (Node ls) (Leaf r ) = compare (Node ls) (Node [Leaf r])

part2trees :: [Tree]
part2trees = [Node [Node [Leaf 2]], Node [Node [Leaf 6]]]

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day13.in"
  print $ sum . map snd $ filter ((==LT) . fst) . flip zip [1..] $ map (uncurry compare) input
  let part2 = sort $ concatMap (\(x,y) -> [x,y]) input <> part2trees
  print $ product . map (+1) <$> traverse (`elemIndex` part2) part2trees

parseInput :: String -> [(Tree, Tree)]
parseInput = map (tuple . either (error . show) id . traverse (parse p "") . lines) . splitOn "\n\n"
  where
    p = Node <$> between (char '[') (char ']') f
    f = sepBy (p <|> (Leaf . read <$> many1 digit)) (char ',')

-- 5659
-- 22110
