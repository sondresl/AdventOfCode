module Day21 where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens ((<&>))

type Op = Int -> Int -> Int
type Monkeys = Map String (Either Int (String, Op, String))

compute :: Monkeys -> String -> Int
compute mp = (res Map.!)
  where res = mp <&> \case 
                       Left i -> i
                       Right (x, op, y) -> (res Map.! x) `op` (res Map.! y)

part2 :: Monkeys -> Int
part2 mp = go 0 100000000000
  where
    root mp = let Right (a,_,b) = mp Map.! "root"
               in (compute mp a, compute mp b)
    go lower step = 
      let (a, b) = root $ Map.insert "humn" (Left lower) mp
       in case a `compare` b of
            EQ -> lower
            LT -> go (lower - step) (step `div` 10)
            GT -> go (lower + step) step

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day21.in"
  print $ compute input "root"
  print $ part2 input

parseInput :: String -> Monkeys
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
