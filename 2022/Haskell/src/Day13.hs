module Day13 where

import Lib (tuple)
import Data.List.Extra (splitOn, elemIndex, sort)
import Text.ParserCombinators.Parsec (between, char, sepBy, many1, digit, parse, (<|>))

newtype Tree = Tree [Either Int Tree]
  deriving (Show, Eq)

instance Ord Tree where
  compare = solve

solve :: Tree -> Tree -> Ordering
solve (Tree ls) (Tree rs) = f ls rs
  where
    f [] (_:_) = LT
    f _ [] = GT
    f (Left l:ls)  (Left r:rs)  = l `compare` r <> f ls rs
    f (Right l:ls) (Right r:rs) = solve l r <> f ls rs
    f (Left l:ls)  (Right (Tree r):rs) = f [Left l] r <> f ls rs
    f (Right (Tree l):ls) (Left r:rs)  = f l [Left r] <> f ls rs

part2trees :: [Tree]
part2trees = [Tree [Right (Tree [Left 2])], Tree [Right (Tree [Left 6])]]

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day13.in"
  print $ sum . map snd $ filter ((==LT) . fst) . flip zip [1..] $ map (uncurry compare) input
  let part2 = sort $ concatMap (\(x,y) -> [x,y]) input <> part2trees
  print $ product . map (+1) <$> traverse (`elemIndex` part2) part2trees

parseInput :: String -> [(Tree, Tree)]
parseInput = map (tuple . either (error . show) id . traverse (parse p "") . lines) . splitOn "\n\n"
  where
    p = Tree <$> between (char '[') (char ']') f
    f = sepBy ((Right <$> p) <|> (Left . read <$> many1 digit)) (char ',')

-- 5659
-- 22110
