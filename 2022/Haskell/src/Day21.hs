module Day21 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.MemoTrie (memo)

type Monkeys = Map String (Either Int (String, Int -> Int -> Int, String))

compute :: Monkeys -> String -> Int
compute mp = calc
  where
    calc = memo $ \x -> case mp Map.! x of
               Left i -> i
               Right (x, op, y) -> calc x `op` calc y

compute2 :: Monkeys -> (Bool, Int, Int)
compute2 mp = let (x, y) = (calc a, calc b) in (x == y, x, y)
  where
    Right (a,_,b) = mp Map.! "root"
    calc = memo $ \x -> case mp Map.! x of
               Left i -> i
               Right (x, op, y) -> calc x `op` calc y

part2 :: Monkeys -> Int
part2 mp = go 0 100000000000  -- assume greater than 2
  where
    go lower move = 
      let (res, a, b) = compute2 $ Map.insert "humn" (Left lower) mp
       in case a `compare` b of
            EQ -> lower
            LT -> go (lower - move) (move `div` 10)
            GT -> go (lower + move) move

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day21.in"
  print $ compute input "root"
  print $ part2 input

parseInput :: String -> Map String (Either Int (String, Int -> Int -> Int, String))
parseInput = Map.fromList . map (f . words) . lines
  where
    f [init -> name, num] = (name, Left (read num))
    f [init -> name, t1, op, t2] = (name, Right (t1, op', t2))
      where op' = case head op of
                    '-' -> (-)
                    '+' -> (+)
                    '*' -> (*)
                    '/' -> div

-- 169525884255464
-- 3247317268284
